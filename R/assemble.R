#' Assemble fusionACS microdata
#'
#' Assemble a fusionACS microdata pseudo-sample, given a user's requested variable(s), year(s), and respondent type. It also allows modification of the microdata via arbitrary expressions passed to [mutate][dplyr::mutate], [filter][dplyr::filter], and [select][dplyr::select] and computed efficiently via \href{https://arrow.apache.org/docs/r/reference/acero.html}{Arrow dplyr queries}.
#'
#' @param variables Vector of quoted or unquoted survey variables to return. In addition to \code{variables}, the output microdata also includes universal identifier variables (see Details).
#' @param respondent Character. Whether to return "household" or "person" microdata; i.e. the type of survey respondent. When \code{respondent = "household"}, any person-level \code{variables} return the response for the head of household (i.e. reference person). When \code{respondent = "person"}, any household-level \code{variables} are replicated for each person within a household.
#' @param ... Optional expressions passed to [mutate][dplyr::mutate] to create new columns, [filter][dplyr::filter] to subset rows, or [select][dplyr::select] to remove variables (usually after a mutate). See Examples.
#' @param year Either 'auto' (the default) or an integer vector specifying the year(s) of ACS-PUMS microdata to return. \code{year = "auto"} automatically sets a plausible value based on the requested \code{variables}.
#' @param directory Character. Path to the local fusionACS data directory. This is typically created automatically by \link{get_microdata}.
#' @param cores Integer. Number of cores used for multithreading in [arrow][arrow::arrow] operations when assembling microdata. The default is one less than the total available cores.
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
#'                 respondent = "household")
#'
#' # Same as above but for years 2017-2019 and with optional expressions used to:
#' # 1. Restrict to households consuming natural gas in the state of Texas
#' # 2. Create a new variable (btung_per_ft2) measuring consumption per square foot
#' # 3. Remove btung and totsqft_en after creating btung_per_ft2
#'test <- assemble(variables = c(hincp, np, btung, totsqft_en, state_name, county10, tract10),
#'                 respondent = "household",
#'                 btung > 0,
#'                 state_name == "Texas",
#'                 btung_per_ft2 = btung / totsqft_en,
#'                 -c(btung, totsqft_en))
#' @export

#-----------
#
# library(rlang)
# library(tidyverse)
# library(fusionModel)
# library(data.table)
# library(collapse)
# library(arrow)

# variables = c('agep', 'wkhp', 'puma10')
# respondent = "person"
# year = 2015:2019
# force_up = TRUE

# # Variables I want:
# variables <- c('state_name', 'cookfuel', 'ratinghs', 'hrfs12m1', 'gsyrgal', 'dollarel', 'hincp', 'rac1p')
# year <- "auto"
# respondent <- "household"
# directory = get_directory()
# cores = get_cores()
# #
# # # !And imagine we have a filter call in assemble() like:
# fun <- function(...) separate_dots(enquos(...))
# dots <- fun()

#----

assemble <- function(variables,
                     respondent,
                     ...,
                     year = "auto",
                     directory = get_directory(),
                     cores = get_cores(),
                     force_up = FALSE
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
  if (inherits(variables[1], "try-error") | variables[1] == "") cli_abort("Missing or invalid 'variables' argument")

  # Check the 'path' argument
  # Remove leading slash from string (normalizePath doesn't like it)
  path <- try(normalizePath(directory, winslash = "/", mustWork = TRUE), silent = TRUE)
  if (inherits(path, "try-error")) cli_abort(c("Could not resolve the 'directory' path:", path))

  # Attempt to assemble dictionary
  dict <- try(read_parquet(file.path(path, "dictionary.parquet")), silent = TRUE)
  if (inherits(dict, "try-error")) cli_abort(c("Failed to access the database dictionary. Are you sure 'directory' is correct?", dict))

  # Check the 'year' argument
  if (year[1] != "auto" & !all(year %in% unlist(dict$years))) cli_abort("Invalid 'year' argument")

  # Check the 'respondent' argument
  if (!length(respondent) == 1 | !respondent[1] %in% c('household', 'person')) cli_abort("Invalid 'respondent' argument")
  rtype <- ifelse(respondent == "household", "H", "P")

  # Check and set the 'cores' argument
  max.cores <- max(parallelly::availableCores(logical = FALSE), parallelly::availableCores(logical = TRUE))
  if (!length(cores) == 1 | !cores[1] %in% 1:max.cores) cli_abort("Invalid 'cores' argument")
  arrow::set_cpu_count(cores)
  data.table::setDTthreads(cores)
  collapse::set_collapse(nthreads = cores)

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
    cli_abort(c("Invalid select() expressions in `...`. Only negations (-) are allowed:",
                paste(dots$invalid, collapse = "\n")))
  }

  # Check for circular mutate() expressions
  if (length(mvars) & all(mvars %in% names(mvars))) {
    cli_abort("It looks like the mutate() calls in `...` are circular.")
  }

  # Check if the mutate(), filter(), and select() variables are present in 'variables'
  # The ... expressions can reference universal variables or 'weight' without explicit inclusion in 'variables' argument
  evars <- unique(setdiff(c(mvars, fvars, svars), names(dots$mutate)))
  emiss <- setdiff(evars, c(variables, uvar, 'weight'))
  if (length(emiss)) {
    cli_abort(c("The following variable(s) are needed by expressions in `...` but are not included in the 'variables' argument:",
                paste(emiss, collapse = "\n"),
                "For safety and clarity, you must explicitly include these in the 'variables' argument."))
  }

  # If no select() expressions, return all variables in result data frame
  if (length(dots$select) == 0) dots$select <- list(expr(everything()))

  #---

  # TO DO: FIX FOR explicit selector!!!
  # Check that the 'variables' are present in the dictionary/database
  vmiss <- setdiff(variables, dict$variable)
  if (length(vmiss)) {
    cli_abort(c("The following variable(s) are not in the database dictionary:",
                paste(vmiss, collapse = "\n")))
  }

  # Determine if UrbanPop weights should be used (use.up = TRUE)
  gn <- names(open_dataset(file.path(path, "geography.parquet")))
  up.gvars <- setdiff(gn, c('region', 'division', 'state', 'state_postal', 'state_name', 'puma10'))  # The geographic variables listed here can utilize native ACS weights
  use.up <- any(variables %in% up.gvars) | force_up

  # Report which weights are being used
  cli::cli_alert(paste0("Returning ", ifelse(use.up, "UrbanPop ", "ACS "), respondent, "-level weights"))

  # If year = "auto", determine appropriate year and reporting result to console
  if (use.up) {
    if (year[1] == "auto") {
      year <- 2015:2019
      cli::cli_alert("Auto-set 'year' argument to 2015:2019 (required for UrbanPop weights)")
    } else {
      if (!identical(sort(year), 2015:2019)) {
        cli::cli_abort(c("The following requested 'variables' require the use of UrbanPop weights:",
                         paste(intersect(variables, up.gvars), collapse = ", "),
                         "This requires the 'year' argument to be set to 'auto' or 2015:2019."))
      }
    }
  } else {
    if (year[1] == "auto") {
      x <- dict %>%
        filter(variable %in% variables) %>%
        mutate(years = lapply(years, na.omit)) %>%
        pull(years) %>%
        purrr::compact()
      year <- ifelse(length(x), max(Reduce(intersect, x)), 2019)
      cli::cli_alert(paste("Auto-set 'year' argument to", year))
    }
  }

  # Check that the 'variables' are available for all of the 'year'
  # If a variable has NA for 'years' in the dictionary, it is allowed (assumed to be a geographic variable)
  ymiss <- dict %>%
    filter(variable %in% variables) %>%
    mutate(year_miss = lapply(years, function(x) if (is.na(x[1])) numeric() else setdiff(year, x))) %>%
    filter(lengths(year_miss) > 0)
  if (nrow(ymiss) > 0) {
    ymiss <- paste(paste0(ymiss$variable, " (", sapply(ymiss$year_miss, paste, collapse = ", "), ")"), collapse = "\n")
    cli_abort(c("The following variable(s) are not available for some of the requested years:",
                paste(ymiss, collapse = "\n")))
  }

  #---

  # Which surveys do the requested variables come from?
  dv <- dict %>%
    filter(!(variable == "puma10" & respondent != !!respondent)) %>%  # Special handling of 'puma10', so that only the appropriate ACS entry is retained
    mutate(selector = paste0("`", survey, ifelse(is.na(vintage), "", paste0("_", vintage)), ":", variable, "`"),
           include = selector %in% variables) %>%
    filter(variable %in% variables | include) %>%
    arrange(variable, survey, desc(vintage)) %>%   # Puts most-recent surveys first for auto-handling of duplicate 'variables'
    group_by(variable) %>%
    mutate(n = n(),
           include = if (any(include)) include else replace(rep(FALSE, n[1]), 1, TRUE))

  # Resolve duplicates?
  if (any(!dv$include)) {
    df <- filter(dv, any(!include))
    cli::cli_alert_warning("The following 'variables' are ambiguous and have been automatically resolved as follows:\n")
    df %>%
      select(variable, survey, vintage, include) %>%
      as.data.frame() %>%
      print(row.names = FALSE)
    ex <- df %>%
      filter(!include) %>%
      pull(selector)
    cli::cli_alert_warning(c("If this is not the intended result, use backticked selector(s) in 'variables'. For example:\n", paste(ex, collapse = ", ")))
  }

  # Restrict to final set of variables to be loaded from disk
  dv <- dv %>%
    filter(include) %>%
    mutate(column = if (n() == 1) variable else selector) %>%
    select(variable, description, survey, vintage, respondent, custom, file, selector, column) %>%
    ungroup()

  #---

  # Paths to required arrow datasets on disk
  # The 'acs.target' ensures that the ACS data for the requested respondent type is included when use.up = FALSE, since 'weight' is always required
  acs.target <- if (use.up) NULL else paste0("ACS_", rtype, ".parquet")
  plist <- dv %>%
    filter(survey != "geography") %>%
    pull(file) %>%
    c(acs.target) %>%
    unique()
  plist <- file.path(directory, plist)

  # Are geographic variables requested (other than 'puma10')?
  use.geo <- "geography" %in% filter(dv, variable != 'puma10')$survey

  dlist <- vector(mode = "list", length = length(plist))
  dlist <- setNames(dlist, basename(plist))

  for (i in seq_along(dlist)) {

    # Open the arrow dataset
    f <- plist[[i]]
    d <- open_dataset(f)

    # Extract the necessary variables
    v <- filter(dv, file == basename(f))
    v <- setNames(v$variable, v$column)

    # Should ACS 'weight' and 'puma10' variables be included?
    # 'puma10' is included to allow merge with 'geography.parquet' when use.up = FALSE
    avar <- if (all(!use.up, basename(f) == acs.target)) {
      c('weight', if (use.geo) 'puma10' else NULL)
    } else {
      NULL
    }

    # This selects the desired variables AND renames them according to column names in 'v'
    # Apparently, select() automatically does the renaming when 'v' is a named vector
    d <- d %>%
      select(any_of(c(uvar, avar, 'bg10')), all_of(v))

    # If applicable, filter on 'year'
    # If input 'year' is all years (2015-2019), skip filter call
    if ('year' %in% names(d)) {
      if (!all(2015:2019 %in% year)) {
        d <- filter(d, year %in% !!year)
      }
    }

    # Add any applicable filter expressions provided by user
    fok <- sapply(dots$filter, function(x) all(all.vars(x) %in% names(d)))
    if (any(fok)) {
      d <- filter(d, !!!dots$filter[fok])
      dots$filter[fok] <- NULL  # Remove the dots filters entry after being added
    }

    # If 'f' is person-level data and respondent == "household, return just the reference person record
    if ('pid' %in% names(d) & respondent != "person") {
      d <- filter(d, pid == 1L)
    }

    # Assign result
    dlist[[i]] <- d

  }

  #---

  # Process any geography...
  # This produces the 'loc' table

  #if (use.geo) {

  g <- open_dataset(file.path(path, "geography.parquet"))
  fok <- sapply(dots$filter, function(x) all(all.vars(x) %in% names(g)))
  geo.filter <- any(fok)
  if (geo.filter) {
    g <- filter(g, !!!dots$filter[fok])
    dots$filter[fok] <- NULL  # Remove the dots filters entry after being added
  }

  # Aggregate 'gfactor', if using UrbanPop weights
  if (use.up) {
    g <- g %>%
      select(bg10, gfactor, any_of(variables)) %>%
      collect()
    gvars <- setdiff(names(g), 'gfactor')
    g <- g %>%
      fgroup_by(gvars) %>%
      fsum()
  } else {
    # If using ACS, simply return 'puma10' and any other geographic variables
    g <- g %>%
      select(puma10, any_of(variables)) %>%
      distinct() %>%
      collect()
  }

  # Safety check
  stopifnot(!anyDuplicated(g))

  # If using UrbanPop weights...
  if (use.up) {

    # Now location and UrbanPop weights
    loc <- open_dataset(file.path(path, "location.parquet")) %>%
      filter(year %in% !!year) %>%
      filter(bg10 %in% g$bg10) %>%
      inner_join(g, by = "bg10") %>%
      collect()

    # Aggregate 'weight'
    gvars <- intersect(names(loc), c(uvar, variables))
    loc <- loc %>%
      fmutate(weight = weight * gfactor * 5) %>%  # NOTE: Multiply by 5 to coerce UrbanPop weights to same overall magnitude as 5 years of PUMS native weights
      get_vars(c(gvars, 'weight')) %>%
      fgroup_by(gvars) %>%
      fsum()

  } else {
    loc <- g
  }

  # Safety check
  stopifnot(!anyDuplicated(loc))

  # Small efficiency improvement by inserting semi_join() within arrow to restrict the data loaded from disk to those
  # observations that can actually me merged to 'out'. Usually merged on year-hid; puma10 in case of PUMS weights.
  if (geo.filter) {

    # Unique combinations of variables in 'out' to retain based on geographic variable(s) requested
    fdata <- loc %>%
      select(all_of(if (use.up) c('year', 'hid') else 'puma10')) %>%
      distinct()

    # Have arrow queries restrict to year-hid returned by geographic filtering
    dlist <- lapply(dlist, function(d) {
      jvars <- intersect(names(d), names(fdata))
      if (length(jvars)) semi_join(x = d, y = fdata, by = jvars) else d
    })
  }

  #}

  #---

  # Do a looped recursive merge that collects the Arrow tables only when necessary (for memory efficiency)
  out <- collect(dlist[[1]]) # Collect just the first Arrow table
  if (length(dlist) > 1) {
    for (i in 2:length(dlist)) {
      by.vars <- Reduce(intersect, list(names(out), names(dlist[[i]]), c(uvar, 'bg10')))
      out <- merge(out, collect(dlist[[i]]), by = by.vars, allow.cartesian = TRUE)
    }
  }

  # This is functionally equivalent to above (all in Arrow) but slower in testing
  # system.time({
  #   out2 <- Reduce(function(x, y) {
  #     by.vars <- Reduce(intersect, list(names(x), names(y), c(uvar, 'bg10')))
  #     inner_join(x, y, by = by.vars)
  #   }, dlist) %>%
  #     collect()
  # })

  #---

  # Add 'M' in case where all variables are from ACS
  # Warning suppression is only data.table warning about a (cheap) shallow copy
  suppressWarnings(if (!'M' %in% names(out)) out[, M := 1L])

  # Check that input dimensions are consistent with one another
  setorder(out, M, year, hid)
  Mv <- out$M
  Mimp <- max(Mv)
  N <- nrow(out) / Mimp
  nM <- collapse::fcountv(Mv)
  stopifnot(!is.unsorted(out$M))
  stopifnot(all(nM$M %in% seq_len(Mimp)))
  stopifnot(all(nM$N == N))

  #----

  # Restrict 'loc' to year-hid in processed 'out'
  # This reduces data a bit prior to merge but is not strictly necessary
  #loc <- loc[out %>% fselect(year, hid) %>% unique(), on = c('year', 'hid'), nomatch = 0]
  #rm(year.hid)

  # Merge/expand 'out' to include variables and weights from 'loc'
  #if (use.geo) {
  setkey(out, NULL)
  out <- merge(out, loc, allow.cartesian = TRUE)
  rm(loc)
  #}

  #----

  # NOTE: I believe the merge() immediately above does the same as this section but has advantage that
  # it also works for case where respondent = 'person'

  # For 'out' and 'loc' data inputs to be compatible, they must have the same number of rows per implicate
  # This section ensures that the row-order of 'out' and 'loc' are in agreement w.r.t 'hid' and 'year'

  # The issue here is that 'loc' can assign a given household (year, hid) to multiple target geographies.
  # However, 'out' contains just one record for each household-implicate (M) combination
  # We need to expand 'out' so that the ordering of households within each implicate matches the ordering within 'loc'

  # # Much faster than match() with manual ID for large data
  # ind <- collapse::fmatch(fselect(loc, year, hid), fsubset(out, M == 1, year, hid), overid = 0)
  #
  # # Safety check
  # stopifnot(!anyNA(ind))
  #
  # # Create row indices for 'sim'
  # isim <- lapply(0:(Mimp - 1), function(x) ind + N * x)  # Expand indices to include all 'Mimp' implicates
  #
  # # Safety check
  # stopifnot(sum(lengths(isim)) / nrow(loc) == Mimp)
  #
  # # Expand 'out' rows
  # # The use of 'isim' ensures 'out' has row ordering that matches 'loc'
  # out <- ss(out, i = unlist(isim), check = FALSE)
  #
  # # Safety checks on dimensions
  # stopifnot({
  #   nrow(out) / nrow(loc) == Mimp
  #   all(loc$hid == out$hid)  # former gets automatically replicated for the check
  #   all(loc$year == out$year) # former gets automatically replicated for the check
  # })
  #
  # loc[, c('year', 'hid') := NULL]
  # rm(ind, isim)
  #
  # # Add 'loc' variables to 'out' to create a single data.table with all required data
  # add_vars(out) <- alloc(loc, Mimp, simplify = FALSE) %>% rbindlist()
  # rm(loc)

  #---

  # Apply any remaining dplyr expressions
  out <- out %>%
    dplyr::mutate(!!!dots$mutate) %>%  # mutate first in case any filtering variables need to be created
    dplyr::filter(!!!dots$filter) %>%
    dplyr::select(!!!dots$select)

  #---

  # Order columns and set keys (all by reference)
  out.cols <- intersect(c(uvar, 'weight', variables, names(dots$mutate)), names(out))
  if (rtype == "H") out.cols <- setdiff(out.cols, "pid")
  drop.cols <- setdiff(names(out), out.cols)
  out[, (drop.cols) := NULL]
  setcolorder(out, out.cols)
  setkeyv(out, cols = intersect(uvar, names(out)))

  #---

  # Print summary of variables returned
  # TO DO: Add mutate vars???  names(dots$mutate))
  info <- dv %>%
    slice(match(names(out), column)) %>%
    mutate(custom = replace_na(custom, FALSE),
           label = paste0(" [Source: ", survey, " ", replace_na(vintage, ""), "]"),
           label = paste0(description, ifelse(survey == "geography", "", label)),
           label = ifelse(custom, paste0(label, " custom variable"), label),
           label = gsub(" ]", "]", label, fixed = TRUE)) %>%
    select(column, label, respondent, selector)

  # Add info for any variables created via custom mutate() calls
  mdf <- tibble(column = names(dots$mutate),
                label = paste0(sapply(dots$mutate, as_label), " [created by mutate expression]"),
                respondent = respondent,
                selector = NA)
  info <- rbind(info, mdf)

  # Set 'info' as data.frame attribute
  #attr(out, "call") <- evaluated_call()  # THIS doesn't behave as expected
  attr(out, "info") <- info

  # Set column labels right before returning result
  out <- labelled::set_variable_labels(.data = out,
                                       .labels = setNames(as.list(info$label), info$column),
                                       .strict = FALSE)

  return(out)

}

#---------
#---------

# separate_dots() from fusionACS::assemble()

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

  # Separate '&' filter() expressions into separate quosures
  filter.exprs <- lapply(unnamed[classes == "filter"], split_quosures_recursive)

  # Return result
  list(
    mutate = named,
    filter = unlist(filter.exprs),
    select = unnamed[classes == "select"],
    invalid = unnamed[classes == "invalid"]
  )

}

#------------

# Recursively splits a quosure containing one or more '&' operators
# into a list of individual condition quosures.

split_quosures_recursive <- function(quosure_obj) {

  if (!is_quosure(quosure_obj)) {
    stop("Input must be an rlang quosure object.")
  }

  expr <- get_expr(quosure_obj)
  env <- get_env(quosure_obj)

  # Base case: If it's not a call, or not an '&' call, return it as a single element list
  if (!is_call(expr) || call_name(expr) != "&") {
    return(list(quosure_obj))
  }

  # Recursive step: If it's an '&' call
  lhs_expr <- expr[[2]]
  rhs_expr <- expr[[3]]

  # Recursively split the left-hand side
  lhs_quosures <- split_quosures_recursive(new_quosure(lhs_expr, env = env))

  # Recursively split the right-hand side (though for typical '&' chains, RHS is usually atomic)
  rhs_quosures <- split_quosures_recursive(new_quosure(rhs_expr, env = env))

  # Combine the results
  return(c(lhs_quosures, rhs_quosures))

}

#------------

# evaluated_call <- function() {
#   call <- match.call(definition = sys.function(sys.parent()), call = sys.call(sys.parent()), expand.dots = FALSE)
#   fn <- sys.function(sys.parent())
#   formals_list <- formals(fn)
#   eval_env <- parent.frame()
#
#   args <- list()
#   for (arg in names(formals_list)) {
#     if (arg == "...") {
#       args[[arg]] <- call[["..."]]
#     } else if (arg %in% names(call)) {
#       args[[arg]] <- eval(call[[arg]], envir = eval_env)
#     } else {
#       args[[arg]] <- eval(formals_list[[arg]], envir = eval_env)
#     }
#   }
#
#   as.call(c(call[[1]], args))
# }
