#' Assemble fusionACS microdata
#'
#' Assemble a fusionACS microdata pseudo-sample, given a user's requested variable(s), year(s), and respondent type. It also allows modification of the microdata via arbitrary expressions passed to [mutate][dplyr::mutate], [filter][dplyr::filter], and [select][dplyr::select] and computed efficiently via \href{https://arrow.apache.org/docs/r/reference/acero.html}{Arrow dplyr queries}.
#'
#' @param variables Vector of quoted or unquoted survey variables to return. In addition to \code{variables}, the output microdata also includes universal identifier variables (see Details).
#' @param year Integer vector specifying the year(s) of ACS-PUMS microdata to return.
#' @param respondent Character. Whether to return "household" or "person" microdata; i.e. the type of survey respondent. When \code{respondent = "household"}, any person-level \code{variables} return the response for the head of household (i.e. reference person). When \code{respondent = "person"}, any household-level \code{variables} are replicated for each person within a household.
#' @param ... Optional expressions passed to [mutate][dplyr::mutate] to create new columns, [filter][dplyr::filter] to subset rows, or [select][dplyr::select] to remove variables (usually after a mutate). See Examples.
#' @param directory Character. Path to the local fusionACS data directory. This is typically created automatically by \link{get_microdata}.
#' @param cores Integer. Number of compute cores used by [arrow][arrow::arrow] when assembling microdata. The default is one less than the total available cores.
#'
#' @return A keyed [data.table][data.table::data.table] containing the requested \code{variables}, as well as the following universal variables (always returned):
#' \describe{
#'   \item{year}{Year of the ACS-PUMS microdata observation.}
#'   \item{hid}{Household ID. Along with \code{year}, this uniquely identifies each ACS-PUMS respondent household.}
#'   \item{pid}{Person ID (if \code{respondent = "person"}). Along with \code{year} and \code{hid}, this uniquely identifies each ACS-PUMS respondent person.}
#'   \item{weight}{The ACS-PUMS central observation weight.}
#' }
#'
#' @examples
#'# Load household income (hincp), household size (np), and state from ACS,
#'#  plus natural gas consumption (btung) and square footage (totsqft_en) from RECS,
#'#  plus pseudo-assignment of county and tract from UrbanPop.
#'# Nationwide household data for ACS year 2019
#'test <- assemble(variables = c(hincp, np, btung, totsqft_en, state_name, county10, tract10),
#'                 year = 2019,
#'                 respondent = "household")
#'
#' # Same as above but for years 2017-2019 and with optional expressions used to:
#' # 1. Restrict to households consuming natural gas in the state of Texas
#' # 2. Create a new variable (btung_per_ft2) measuring consumption per square foot
#' # 3. Remove btung and totsqft_en after creating btung_per_ft2
#'test <- assemble(variables = c(hincp, np, btung, totsqft_en, state_name, county10, tract10),
#'                 year = 2017:2019,
#'                 respondent = "household",
#'                 btung > 0,
#'                 state_name == "Texas",
#'                 btung_per_ft2 = btung / totsqft_en,
#'                 -c(btung, totsqft_en))
#' @export

# test <- assemble(variables = c(state, hincp, np, schl, dollarel, dollarng),
#                  year = 2019,
#                  respondent = 'household',
#                  # Optional subsetting expressions
#                  state == '48', # filter() expression
#                  schl >= "Bachelor's degree", # filter() expression
#                  equivalized_income = hincp / sqrt(np), # mutate() expression
#                  energy_burden = (dollarel + dollarng) / hincp, # mutate() expression
#                  -c(hincp, np, dollarel, dollarng))  # negating select() expression
#
# dim(test)
#
#
# test2 <- assemble(variables = c(state, hincp, np, schl),
#                   year = 2019,
#                   respondent = 'household')
#
# dim(test2 %>% filter(state == '48', schl >= "Bachelor's degree"))

#-----

assemble <- function(variables,
                     year,
                     respondent,
                     ...,
                     directory = get_directory(),
                     cores = get_cores()
) {

  # Check and construct the 'variables' argument
  variables <- try({
    v <- enquo(variables)
    if (rlang::quo_is_call(v, "c")) {
      exprs <- as.list(rlang::quo_get_expr(v))[-1]  # remove the 'c'
      sapply(exprs, rlang::as_string)
    } else {
      rlang::as_string(rlang::quo_get_expr(v))
    } %>%
      unique()  # Eliminate any duplicate entries
  }, silent = TRUE)
  if (inherits(variables[1], "try-error") | variables[1] == "") stop("Missing or invalid 'variables' argument")

  # Check the 'year' argument
  year <- try(as.integer(year), silent = TRUE)
  if (inherits(year, "try-error") | !all(year > 2000)) stop("Invalid 'year' argument")

  # Check the 'respondent' argument
  if (!length(respondent) == 1 | !tolower(respondent[1]) %in% c('household', 'person')) stop("Invalid 'respondent' argument")
  rtype <- ifelse(respondent == "household", "H", "P")

  # Check the 'cores' argument
  if (!length(cores) == 1 | !cores[1] %in% 1:arrow::cpu_count()) stop("Invalid 'cores' argument")
  arrow::set_cpu_count(cores)

  # Universal variables always returned in output, if possible
  uvar <- c('M', 'year', 'hid', 'pid')

  #---

  # Capture any expressions passed via ... argument
  # Determine whether each expression should be passed to mutate(), filter(), or select()

  dots <- separate_dots(enquos(...))
  mvars <- unlist(lapply(dots$mutate, function(x) all.vars(rlang::quo_get_expr(x))))
  fvars <- unlist(lapply(dots$filter, function(x) all.vars(rlang::quo_get_expr(x))))
  svars <- unlist(lapply(dots$select, function(x) all.vars(rlang::quo_get_expr(x))))

  # If there is an invalid select() expression, report with as error
  if (length(dots$invalid)) {
    stop("Invalid select() expressions in `...`. Only negations (-) are allowed:\n", paste(dots$invalid, collapse = "\n"))
  }

  # Check for circular mutate() expressions
  if (length(mvars) & all(mvars %in% names(mvars))) stop("It looks like the mutate() calls in `...` are circular.")

  # Check if the mutate(), filter(), and select() variables are present in 'variables'
  # The ... expressions can reference universal variables or 'weight' without explicit inclusion in 'variables' argument
  evars <- unique(setdiff(c(mvars, fvars, svars), names(dots$mutate)))
  emiss <- setdiff(evars, c(variables, uvar, 'weight'))
  if (length(emiss)) stop("The following variable(s) are needed by expressions in `...` but are not included in the 'variables' argument:\n", paste(emiss, collapse = "\n"), "\nFor safety and clarity, you must explicitly include these in the 'variables' argument.")

  # If no select() expressions, return all variables in result data frame
  if (length(dots$select) == 0) dots$select <- list(expr(everything()))

  #---

  # Check the 'path' argument
  # Remove leading slash from string (normalizePath doesn't like it)
  path <- try(normalizePath(directory, winslash = "/", mustWork = TRUE), silent = TRUE)
  if (inherits(path, "try-error")) stop("Could not resolve the 'directory' path: ", path)

  # Attempt to assemble dictionary
  dict <- try(fusionACS::dictionary(directory = path), silent = TRUE)
  if (inherits(dict, "try-error")) stop("Failed to access the database dictionary. Are you sure 'database' is correct?\n", dict)

  # Check that the 'variables' are present in the dictionary/database
  vmiss <- setdiff(variables, dict$variable)
  if (length(vmiss)) stop("The following variable(s) are not in the database dictionary:\n", paste(vmiss, collapse = "\n"))

  # Check that the 'variables' are available for all of the 'year'
  ymiss <- dict %>%
    filter(variable %in% variables) %>%
    mutate(year_miss = lapply(years, function(x) setdiff(year, x))) %>%
    filter(lengths(year_miss) > 0)
  if (nrow(ymiss) > 0) {
    ymiss <- paste(paste0(ymiss$variable, " (", sapply(ymiss$year_miss, paste, collapse = ", "), ")"), collapse = "\n")
    stop("The following variable(s) are not available for some of the requested years:\n", paste(ymiss, collapse = "\n"))
  }

  #---

  # Get directories of ACS Arrow datasets (ordered by priority)
  adir <- list.files(path, pattern = "^ACS_", full.names = TRUE)
  adir <- adir[order(str_sub(adir, -9, -9) != rtype)]

  # Get directories of donor survey Arrow datasets (ordered by priority)
  midyear <- mean(as.numeric(year))
  ddir <- setdiff(list.files(path, pattern = "[HP].parquet$", full.names = TRUE), adir)
  #ddir <- grep("^(?!.*year=\\d{4}$).*", ddir, perl = TRUE, value = TRUE)
  ddir <- sort(ddir, decreasing = TRUE)
  ddir <- ddir[order(abs(as.numeric(str_sub(basename(ddir), -14, -11)) - midyear), str_sub(basename(ddir), -9, -9) != rtype)]

  dlist <- c(ddir, adir)
  v <- c(variables, 'weight')
  out <- NULL

  for (s in dlist) {
    r <- str_sub(basename(s), -9, -9)
    d <- open_dataset(s)
    if (any(v %in% names(d))) {
      d <- d %>%
        select(any_of(c(uvar, v))) %>%
        filter(year %in% !!year)
      if (r == "P" & rtype != "P") {
        d <- filter(d, pid == 1L)  # Retains only the reference person record
      }
      out[[s]] <- d
      v <- setdiff(v, names(d))
      if (length(v) == 0) break()
    }
  }

  # Perform successive left joins, apply optional mutate() and filter() expressions, and collect the data
  out <- Reduce(function(x, y) {
    by.vars <- Reduce(intersect, list(names(x), names(y), uvar))
    left_join(x, y, by = by.vars)
  }, out) %>%
    dplyr::mutate(!!!dots$mutate) %>%  # mutate first in case any filtering variables need to be created
    dplyr::filter(!!!dots$filter) %>%
    #dplyr::select(!!!dots$select) %>%  # Moved to after collect(); see explanation below
    collect()

  # When arrow executes dplyr::filter(), it seems to work fine for == or %in% operators but not for <> on ordered factors.
  # This doesn't appears to be an explicitly know bug, but similar issues have been noted with factors:
  # See here: https://github.com/apache/arrow/pull/43446
  # And here: https://github.com/apache/arrow/issues/40430

  # NOTE: I think the issue may have something do with arrow's filter() following the recursive merged
  # Small scale testing suggests <> operations on ordered factors generally work

  # This is a kluge to ensure that any <> filter() operations are enforced after arrow collect(), just in case.
  # It requires that select() not be called by arrow but only later, since columns may be needed to enforce the filter() conditions below
  # This assumes that filter() calls by arrow are not incorrect -- only incomplete in the case of <> operators on ordered factors
  out <- out %>%
    dplyr::filter(!!!dots$filter) %>%
    dplyr::select(!!!dots$select)

  # Convert to data.table, add 'M' placeholder column, order columns, and set keys (all by reference)
  out <- as.data.table(out)
  out.cols <- intersect(c(uvar, 'weight', variables, names(dots$mutate)), names(out))
  if (rtype == "H") out.cols <- setdiff(out.cols, "pid")
  out <- out[, ..out.cols]
  setkeyv(out, cols = intersect(uvar, names(out)))

  return(out)

}

#---

# assemble(year = 2019, respondent = 'household', variables = c(dollarel), myvar = dollarel ^ 2)
#
# assemble(year = 2015:2019, respondent = 'household', variables = c(xxx, sss), database = "p")
#
# assemble(year = 2015:2019, respondent = 'household', variables = c(xxx, sss), x > 15, y > 13)
#
# assemble(x = g + 1, year = 2015:2019,respondent = 'household', variables = c(xxx, sss))
#
# assemble(x = g + 1, g = v + 1, year = 2015:2019,respondent = 'household', variables = c(xxx, sss), x > 15)
#
# assemble(x = g + 1, g = v + 1, v = 16, year = 2015:2019,respondent = 'household', variables = c(xxx, sss), x > 15)
#
# assemble(x = x ^ 2, x = g + 1, g = v + 1, v = 16, year = 2015:2019,respondent = 'household', variables = c(xxx, sss), x > 15)
#
#
# # TEST! - It appears that assemble() wants all of the arguments specified
# test <- assemble(variables = dollarel, year = 2015, newvar = dollarel * 2, dollarng > 1000)
#
# test <- assemble(variables = dollarel, year = 2015, respondent = "household", dollarel > 1000, rac1p == "White alone")
#
# test <- assemble(year = 2015, variables = c(hincp, wagp), electricity_price = dollarel / btuel, rac1p == "White alone", np > 2)
#
# test <- assemble(year = 2015, variables = c(hincp, wagp))

#--------

separate_dots <- function(dots) {
  named <- dots[nzchar(names(dots))]
  unnamed <- dots[!nzchar(names(dots))]

  is_symbol_list <- function(expr) {
    is.call(expr) && identical(expr[[1]], quote(c)) &&
      all(vapply(as.list(expr)[-1], is.symbol, logical(1)))
  }

  classify_expr <- function(quo) {
    expr <- rlang::quo_get_expr(quo)
    if (is.call(expr) && identical(expr[[1]], quote(`-`))) {
      arg <- expr[[2]]
      if (is.symbol(arg) || is_symbol_list(arg)) return("select")
    }
    if (is.symbol(expr) || is_symbol_list(expr)) return("invalid")
    "filter"
  }

  classes <- vapply(unnamed, classify_expr, character(1))

  list(
    mutate = named,
    filter = unnamed[classes == "filter"],
    select = unnamed[classes == "select"],
    invalid = unnamed[classes == "invalid"]
  )
}

#---

# Helper function for returning directories (can move eventually)
list_child_dirs <- function(path, steps_down = 1) {
  path <- normalizePath(path, mustWork = TRUE)
  base_depth <- length(strsplit(path, .Platform$file.sep)[[1]])
  dirs <- list.dirs(path, full.names = TRUE, recursive = TRUE)
  # Filter: keep only directories that are 'steps_down' levels deeper
  dirs_to_keep <- dirs[
    sapply(strsplit(dirs, .Platform$file.sep), length) == (base_depth + steps_down)
  ]
  return(dirs_to_keep)
}
