#' Analyze fusionACS microdata
#'
#' @description
#' Analyze a fusionACS microdata pseudo-sample created by \link{assemble}. Efficiently computes means, medians, sums, proportions, and counts of specified variables, optionally across population subgroups.
#'
#' @param data Data frame. fusionACS microdata pseudo-sample returned by \link{assemble}.
#' @param ... Formulas. Used to define the desired analyses. See Examples.
#' @param by Optional variable(s) that collectively define the set of population subgroups for which each analysis is computed. Can be a mix of geographic (e.g. census tract) and/or socio-demographic microdata variables (e.g. poverty status); the latter may be existing variables on disk or custom variables created on-the-fly via \code{fun()}. If \code{NULL}, analysis is done for the whole (national) sample.
#' @param fun Function. Optional function for modifying \code{data} prior to analysis.
#' @param cores Integer. Number of cores used for multithreading in \code{\link[collapse]{collapse-package}} functions. The default is one less than the total available cores.
#'
# Required for full computation but not necessary for pseudo-microdata
# @param M Integer. The first \code{M} implicates are used. Set \code{M = Inf} to use all available implicates.
# @param R Integer. The first \code{R} replicate weights are used. Set \code{R = Inf} to use all available replicate weights.
# @param version_up Integer. Use \code{version_up = 2} to access 10-replicate weights for 17 metro areas or \code{version_up = 3} to access 40-replicate weights for 17 metro areas.
# @param force_up Logical. If \code{TRUE}, force use of UrbanPop weights even if the requested analysis can be done using native ACS weights.
#'
#' @examples
#'my.data <- assemble(
#'  variables = c(hincp, np, btung, totsqft_en, acequipm_pub, state_name, county10, tract10),
#'  year = 2019,
#'  respondent = "household",
#'  state_name == "Texas"
#')
#'
#' test <- analyze(
#'   data = my.data,
#'   mean_btung ~ mean(btung),
#'   ~median(totsqft_en),
#'   ~mean(acequipm_pub),
#'   by = list(np, c(state_name, county10, tract10))
#' )
#' @export

#---------------

# library(rlang)
# library(tidyverse)
# library(collapse)
# library(data.table)
#
# # utils
# source("~/Documents/Projects/fusionACS/R/zzz.R")

# # Get some microdata for test
# data <- fusionACS::assemble(variables = c(rac1p, hht, hincp, wagp, elep, rmsp, state, np, region),
#                             year = 2017:2019,
#                             respondent = "household")
#
#
# test <- analyze(data, myvar ~ mean(hincp), ~median(hincp), ~mean(np), ~mean(hht), ~median(np), ~mean(wagp), ~median(wagp), ~mean(elep), ~median(elep), by = c(rmsp, state))
#
# # Any patterns in ubar? Yes!
# check <- test %>%
#   mutate(n = ifelse(is.na(level), N_eff, N_eff * est),
#          ubar_cv = sqrt(ubar) / abs(est))
#
# plot(check$n, check$ubar_cv)
# plot(log(check$n), log(check$ubar_cv))
#
# # Only numeric analyses (mean and median)
# ncheck <- filter(check, is.na(level))
# plot(log(ncheck$n), log(ncheck$ubar_cv))
#
# # Only categorical analyses (proportion)
# ccheck <- filter(check, !is.na(level))
# plot(log(ccheck$n), log(ccheck$ubar_cv))
#
# fit <- lm(log(ubar_cv) ~ log(n) + type, data = filter(check, n > 0, ubar_cv > 0, is.finite(ubar_cv)))
# summary(fit)
#
# # Numeric outcomes
# plot(log(1 + check$n), log(1 + check$ubar_cv))
#
# fit <- lm(log(ubar_cv) ~ log(n) + type, data = filter(check, n > 0, ubar_cv > 0, is.finite(ubar_cv), is.na(level)))
# summary(fit)
#
# test <- analyze(data, myvar ~ mean(rac1p), ~sum(hht), ~mean(np), ~median(np), by = list(state, c(state, region)))

#---------------

# data <- fusionACS::assemble(variables = c(rac1p, hht, hincp, dollarel, state, np, region),
#                             year = 2017:2019,
#                             respondent = "household")
#
# test <- analyze(data, myvar ~ mean(rac1p), ~sum(hht), ~mean(hincp), ~median(dollarel), by = list(state, c(np, region)))
#
# test <- analyze(data, ~mean(hincp), ~mean(dollarel), ~mean(np), by = list(state, c(rac1p, region)))
#
# # Duplicates; should fail
# test <- analyze(data, mean = c(hincp, dollarel), ~mean(hincp), by = list(state, c(np, region)))
#
# test <- analyze(data, mean = c(hincp, dollarel), median = c(hincp,    dollarel  ), ~mean(hht), ~sum(rac1p), by = list(state, c(np, region)))
#
# # RELATIONSHIP?
#
# # Numeric case
# plot(log(test$n_eff), log(test$SE / test$EST))
#
# # Categoric case
# temp <- filter(test, grepl("^mean", ANALYSIS))
# plot(log(temp$n_eff * temp$EST), log(temp$SE / temp$EST))

#-------

analyze <- function(data,
                    ...,
                    by = NULL,
                    fun = NULL,
                    cores = get_cores()

) {

  # # Check validity of the working directory path
  # # Checks if "/fusionData" is part of the path, as this is required
  # b <- strsplit(full.path(getwd()), .Platform$file.sep, fixed = TRUE)[[1]]
  # i <- which(b == "fusionData")
  # if (length(i) == 0) stop("'/fusionData' is not part of the working directory path; this is required.")
  # dir <- paste(b[1:i], collapse = .Platform$file.sep)
  #
  # # Capture the function call; added as attribute in the final output
  # mcall <- match.call()
  #
  # t0 <- Sys.time()
  # fst::threads_fst(nr_of_threads = cores)
  # setDTthreads(threads = cores)

  # Set collapse package option to silently ignore inapplicable 'nthreads' argument
  # See '...' entry in ?collap; applicable to fsd() and fvar()
  options(collapse_unused_arg_action = "none")

  # # Check the 'area' expression to determine how to parse it
  # # If 'area' is passed a 'call', it is not modified
  # # This is useful for special user input like: area = str2lang(paste0("state == '", st.obj, "'"))
  # # The more common case is for 'area' to follow usage like in subset()
  # # See here: http://adv-r.had.co.nz/Computing-on-the-language.html
  # check <- try(is.call(area), silent = TRUE)
  # if (inherits(check, "try-error") | check == FALSE) {
  #   #if (!is.null(area)) {
  #   area <- substitute(area)
  #   if (is.character(area)) area <- str2lang(area)
  #   #}
  # }
  # area.vars <- all.vars(area)

  # Respondent identifier ("H" or "P")
  #rtype <- substring(toupper(respondent), 1, 1)

  # # Initial argument check
  # method <- tolower(method)
  # stopifnot({
  #   rlang::is_formula(analyses) | is.list(analyses)
  #   rtype %in% c("H", "P")
  #   all(year >= 2005) & all(diff(year) == 1)
  #   is.null(by) | is.character(by)
  #   is.null(area) | is.call(area)
  #   is.null(fun) | is.function(fun)
  #   M >= 1 & M %% 1 == 0
  #   R >= 0 & R %% 1 == 0
  #   cores > 0 & cores %% 1 == 0
  #   is.logical(force_up)
  #   version_up %in% 2:3
  #   method %in% c("auto", "sdr", "other")
  # })

  #---

  # Prepare and check 'analyses' input
  #if (!is.list(analyses)) analyses <- list(analyses)

  # Capture the analysis formulas from ... argument
  analyses <- collect_formulas(...)

  # TESTING
  #analyses = list(myvar ~ mean(rac1p))
  #analyses = list(myvar ~ mean(np), ~median(hincp))
  #print(analyses)

  # analyses <- lapply(analyses, function(q) as.formula(rlang::quo_squash(q)))
  #
  # # FIX THIS
  # # Attempt to convert any non-formula entries in 'analyses' into a plausible formula
  # # This applies to legacy analysis formulation of the kind:
  # #  analyses <- list(mean = c("natural_gas", "aircon"), median = "electricity")
  # # The code below converts these to an equivalent formula and assigns LHS as concatenation of analysis variable name and outer function
  # analyses <- lapply(seq_along(analyses), function(i) {
  #   x <- analyses[[i]]
  #   if (!rlang::is_formula(x)) {
  #     f <- names(analyses)[i]  # The requested outer function
  #     fobj <- paste0(gsub(" ", "_", str_squish(x)), "_", f, "~", f, "(`", x, "`)")
  #     lapply(fobj, as.formula)
  #   } else {
  #     x"\"a\""
  #   }
  # }) %>%
  #   unlist()

  #---

  # Check that all 'analyses' are formulas
  if (!all(sapply(analyses, rlang::is_formula))) stop("All 'analyses' must be formulas")

  # Remove any duplicate analyses
  dupe <- sapply(analyses, rlang::f_rhs)
  dupe <- unique(dupe[duplicated(dupe)])
  if (length(dupe)) stop("The following analyses are requested multiple times:\n ", paste0(dupe, collapse = "\n "), "\n", sep = "")

  # Check for duplicate analysis names (LHS of formula)
  if (anyDuplicated(purrr::compact(sapply(analyses, rlang::f_lhs)))) stop("Detected duplicate LHS analysis names (must be unique)")

  # Extract LHS (names), functions, and variables from analyses
  afuns <- sapply(analyses, function(x) as_label(f_rhs(x)[[1]]))
  avars <- sapply(analyses, function(x) as_label(f_rhs(x)[[2]]))
  alhs <- sapply(analyses, function(x) as_label(f_lhs(x)))
  arhs <- sapply(analyses, function(x) as_label(f_rhs(x)))

  # Check if the analysis functions are valid
  invalid <- setdiff(afuns, c('mean', 'median', 'sum', 'total', 'prop', 'count'))
  if (length(invalid)) stop("The following analysis functions are not allowed: ", paste(invalid, collapse = ", "))

  #---

  #TESTING
  #blist <- list("np", c("np", "region"))
  #blist <- list("region", c("state", "rac1p"))

  # Parse the 'by' argument
  expr <- substitute(by)
  blist <- if (missing(by) || is.null(expr) || identical(expr, quote(NULL))) {
    list()
  } else {
    parse_by(expr)
  }

  blist <- setNames(blist, lapply(blist, paste, collapse = "__"))
  bvars <- unique(unlist(blist))
  #print(blist)

  # An analysis variable cannot be in 'by'
  err <- intersect(avars, bvars)
  if (length(err)) stop("Analysis variables cannot also be in the 'by' argument: ", paste(err, collapse = ", "))

  # Universal variables that MUST be present in 'data', both before and after fun() is applied
  ureq <- c('year', 'hid', 'pid', 'weight')
  if (all(c('year', 'hid') %in% names(data)) & nrow(data) == uniqueN(data, by = c('year', 'hid'))) ureq <- setdiff(ureq, 'pid')
  miss <- setdiff(ureq, names(data))
  miss <- if ('hid' %in% miss) sub('^pid$', 'pid (possibly)', miss)
  if (length(miss)) stop("The following required variables are missing from 'data':\n", paste(miss, collapse = "\n"))

  #---

  # If user 'fun' is supplied, attempt to apply it to 'data'
  # Note that the input universal variable values are enforced in the output
  # TO DO: For production, fun() should be applied by implicate (M)
  if (!is.null(fun)) {
    cat("Applying user 'fun' to 'data'\n")
    data <- fun(data)
    if (!is.data.frame(data)) stop("The supplied 'fun' does not return a data frame")
  }

  # Check that 'data' has all the required variables
  # This check comes after applying fun(), since the necessary variables could be added via fun()
  miss <- setdiff(c(ureq, avars, bvars), names(data))  # Check for missing required variables, including 'by' variables
  if (length(miss)) {
    if (is.null(fun)) {
      stop("The following required variables are missing from 'data':\n", paste(miss, collapse = "\n"))
    } else {
      stop("The supplied 'fun' does not return the following required variables:\n", paste(miss, collapse = "\n"))
    }
  }

  # Success message
  if (!is.null(fun)) cat("Successfully applied user 'fun' to 'data'\n")

  #---

  # !!!TEMPORARY!!!
  # Put 'weight' into it own 'static' data frame so it can be used with the code below
  static <- get_vars(data, 'weight')
  static <- frename(static, REP__0 = weight)

  # Create 'sim' containing all necessary variables other than the weight variable(s)
  sim <- get_vars(data, unique(c(avars, bvars)))

  rm(data)

  # !!!TEMPORARY!!!
  if (!"M" %in% names(sim)) sim$M <- 1L

  # Convert non-positive weights to NA and convert integer weights to double for subsequent processing speed (at memory expense, though)
  # TO DO: Is this NECESSARY FOR CATEGORICAL ANALYSES or just numeric?
  for (v in names(static)) {
    x <- static[[v]]
    setv(x, x < 0, NA)
    if (is.integer(x)) set(static, j = v, value = as.double(x))
  }

  #---

  # Are all of the required variables available in 'sim'
  miss <- setdiff(c(avars, bvars), names(sim))
  if (length(miss)) stop("The following variables required for analysis were not found in 'sim': ", paste(miss, collapse = ", "))

  # Issue a warning if any of the 'by' variables have >100k unique values
  if (length(bvars)) {
    n <- sapply(get_vars(sim, bvars), uniqueN, na.rm = TRUE)
    if (any(n > 100e3)) {
      warning("The following 'by' variables have greater than 100,000 unique values (is this really what you want?): ", bvars[n > 100e3], immediate. = FALSE)
    }
  }

  # Determine which analyses are categorical and have outer function "mean" or "sum"
  acat <- which(avars %in% cat_vars(sim, return = "names"))
  invalid <- !afuns[acat] %in% c("mean", "prop", "sum", "count")
  if (any(invalid)) stop("Categorical analysis variables must use mean(), prop(), sum(), or count():\n", paste(analyses[invalid], collapse = "\n"))

  # Numerical analyses to be performed
  anum <- setdiff(seq_along(analyses), acat)
  invalid <- !afuns[anum] %in% c("mean", "median", "sum", "total")
  if (any(invalid)) stop("Numerical analysis variables must use mean(), median(), sum(), or total():\n", paste(analyses[invalid], collapse = "\n"))

  #---

  # Create unique grouping variables that are based on more than one variable
  # This creates a unique combination with double-underscore separator
  for (i in seq_along(blist)) {
    if (length(blist[[i]]) > 1) {
      set(sim, j = names(blist)[i], value = do.call(paste, c(get_vars(sim, blist[[i]]), sep = "__")))
    }
  }

  # Remove 'by' variables no longer needed because they are (only) part of a combination variable
  if (length(bvars)) {
    get_vars(sim, setdiff(bvars, names(blist))) <- NULL
  }

  # Coerce 'by' variables to character to avoid any variable type conflicts during melt
  # For example, if the 'by' variables mix integer and factor
  sim[, (names(blist)) := lapply(.SD, as.character), .SDcols = names(blist)]

  # Melt by group variables
  if (length(bvars)) {
    sim <- melt(sim,
                measure.vars = names(blist),
                variable.name = "GRP_NAME",
                value.name = "GRP_VALUE")
  } else {
    # Placeholders if no 'by' argument specified
    sim[, `:=`(GRP_NAME = 1L, GRP_VALUE = 1L)]
  }

  # Compute index vector once for use below
  # This is used within both 'nout' and 'cout' generation; links 'sim' rows to 'static' rows
  ind <- rep.int(seq_row(static), times = nrow(sim) / nrow(static))

  #---

  # Do the categorical variable calculations
  if (length(acat)) {

    cat("Computing estimates for categorical analyses:\n ~", paste(sapply(analyses[acat], rlang::f_text), collapse = "\n ~ "), "\n")

    # Variables in 'sim' needed for categorical analyses
    cvars <- unique(avars[acat])

    # Pull the cvars into its own dataset and then convert to long-format by the group variables

    # FOR TESTING
    # sim <- fusionACS::assemble(variables = c(hincp, rac1p, hht, np, region, state, btung, totsqft_en),
    #                            year = 2019,
    #                            respondent = "household")
    # sim$M <- 1L
    # static <- data.table(REP__0 = sim$weight)
    # cvars <- c("region", "hht")
    # blist <- list("rac1p", c("rac1p", "state"))
    # blist <- setNames(blist, lapply(blist, paste, collapse = "__"))
    # bvars <- unique(unlist(blist))

    # # Create unique grouping variables that are based on more than one variable
    # # This creates a unique combination with double-underscore separator
    # for (i in seq_along(blist)) {
    #   if (length(blist[[i]]) > 1) set(cdata, j = names(blist)[i], value = do.call(paste, c(get_vars(cdata, blist[[i]]), sep = "__")))
    # }
    #
    # # Coerce 'by' variables to character to avoid any variable type conflicts during melt
    # # For example, if the 'by' variables mix integer and factor
    # cdata[, (names(blist)) := lapply(.SD, as.character), .SDcols = names(blist)]

    # Get only the required variables from 'sim'
    #cdata <- get_vars(sim, c("M", cvars, names(blist)))

    # Melt by group variables
    # cdata <- melt(cdata,
    #               measure.vars = names(blist),
    #               variable.name = "GRP_NAME",
    #               value.name = "GRP_VALUE")

    # Compute this after melt to long format
    #ind <- rep.int(seq_row(static), times = nrow(cdata) / nrow(static))

    # Decide whether to use multithreading (via collapse package) or forked processed (via mclapply)
    # On Windows, only multithreading is possible (no forking)
    mt.cores <- ifelse(.Platform$OS.type == "windows" | ncol(static) >= length(cvars), cores, 1L)

    # Calculation explanation (4/7/25):
    # Which quantities are required by the calculation below depends on whether means (proportions), sums (counts), or both are requested for given categorical variable
    # In all cases, we return the number of observations (fnobs) and the sum of weights; they are useful variables in the final function output
    # For proportions, the variance is calculated as p * (1-p) * w2, where w2 is the sum of the squared, scaled frequency weights (see here: https://stats.stackexchange.com/questions/159204/how-to-calculate-the-standard-error-of-a-proportion-using-weighted-data)

    # Main weights data.frame, assuming no NA values in 'sim'
    W0 <- ss(static, i = ind, check = FALSE)

    # Main calculation
    cout <- parallel::mclapply(cvars, function(v) {

      # Create weights object 'W' that is expanded to match nrow(sim) AND has NA values where 'sim' is NA
      # The setting of NA's is necessary to correctly sum the weights ('W') by group
      if (anyNA(sim[[v]])) {
        ok <- whichNA(sim[[v]], invert = TRUE)
        W <- W0[ok]
        grp <- GRP(sim[ok], by = c("M", "GRP_NAME", "GRP_VALUE", v))
      } else {
        W <- W0
        grp <- GRP(sim, by = c("M", "GRP_NAME", "GRP_VALUE", v))
      }

      #----

      # Sum of squared weights, by M-REP-group
      w2sum <- fsum(W ^ 2, g = grp, nthreads = mt.cores) %>%
        add_vars(grp$groups) %>%
        melt(measure.vars = names(W), variable.name = "REP", value.name = "w2sum") %>%
        fmutate(REP = as.integer(gsub("^REP__", "", REP)))

      #----

      # Do grouped calculations (fsum only)
      out <- collapse::fsum(x = W,
                            g = grp,
                            nthreads = mt.cores,
                            use.g.names = FALSE
      )

      out %>%
        add_vars(grp$groups) %>%
        melt(measure.vars = names(W), variable.name = "REP", value.name = "W") %>%
        fmutate(REP = as.integer(gsub("^REP__", "", REP)),
                W = as.integer(W),  # IS THIS REALLY NECESSARY? Computes above on doubles for speed; convert back to integer when complete
                aname = factor(v)) %>%
        join(w2sum, on = c(grp$group.vars, "REP"), verbose = FALSE, overid = 0) %>%
        setnames(old = v, new = "level")

    }, mc.cores = ifelse(mt.cores == 1, cores, 1L)) %>%
      rbindlist()

    # End of initial calculation of 'cout'

    #---

    # TO DO: This needs to do a better job of picking up all potential combinations of groups, even those unobserved

    # Add unobserved combinations, efficiently

    # Complete the data by including zero estimates for unobserved combinations
    # Correct tidyr result (perhaps sorted)
    #cout <- tidyr::complete(cout, tidyr::nesting(!!!rlang::syms(by), M, REP), tidyr::nesting(ANALYSIS, level), fill = list(EST = 0))

    # Alternative approach (faster)
    # https://stackoverflow.com/questions/25888706/r-data-table-cross-join-not-working
    CJ.dt = function(X, Y) {
      k = NULL
      X = X[, c(k=1, .SD)]
      setkey(X, k)
      Y = Y[, c(k=1, .SD)]
      setkey(Y, NULL)
      X[Y, allow.cartesian=TRUE][, k := NULL][]
    }
    u1 <- cout %>%
      get_vars(c("M", "REP", "GRP_NAME", "GRP_VALUE")) %>%
      funique()
    u2 <- cout %>%
      #get_vars(c("ANALYSIS", "level")) %>%
      get_vars(c("aname", "level")) %>%
      funique()
    temp <- CJ.dt(u1, u2)
    temp <- fsetdiff(temp, get_vars(cout, names(temp)))
    if (nrow(temp) > 0) {
      temp <- add_vars(temp,
                       W = alloc(0L, nrow(temp)),
                       #N = alloc(0L, nrow(temp)),
                       w2sum = alloc(0, nrow(temp)))
      cout <- rbind(cout, temp, fill = TRUE)
      rm(temp)
    }

    #---

    # Expand results row-wise to make copies when multiple functions requested for the same 'aname' variable
    # The merge below expands result row-wise to make copies when/if multiple functions are requested for the same analysis variable
    # This produces duplicate records when both proportion and sum are requested (with identical data for the moment; sums are adjusted below)
    amerge <- lapply(cvars, function(v) {
      data.frame(aname = v, ANALYSIS = afuns[avars == v]) %>%
        mutate(ANALYSIS = factor(paste(ANALYSIS, aname, sep = ".")))
    }) %>%
      rbindlist()

    cout <- cout %>%
      join(amerge, on = "aname", multiple = TRUE, verbose = FALSE, overid = 0)

    #---

    # Calculate proportions and effective sample size (n_eff), by group
    # This computes proportions for all observations/rows (overwritten with sums, as necessary, in next step)
    # The sum() calls here work across levels within an analysis
    cout[, `:=`(EST = W / sum(W),
                n_eff = fifelse(any(W > 0), sum(W) ^ 2 / sum(w2sum), 0)),
         by = .(M, REP, ANALYSIS, GRP_NAME, GRP_VALUE)]

    # Calculate the Agresti-Coull adjusted point estimate, assuming a 90% confidence interval (z = 1.644854)
    z <- 1.644854
    cout[, EST_ac := (EST * n_eff + z ^ 2 / 2) / (n_eff + z ^ 2)]

    # Calculate standard error of proportions using the Agresti-Coull adjusted point estimates and effective sample size
    cout[, SE := sqrt(EST_ac * (1 - EST_ac) / n_eff)]

    # For requested sums (i.e. counts), calculate the estimate (total weight) and standard error
    j <- grepl("^sum", cout$ANALYSIS)
    cout[j, EST := W]
    cout[j, SE := SE * sum(W), by = .(M, REP, ANALYSIS, GRP_NAME, GRP_VALUE)]

    # Make the results wide
    cout <- widen_groups(cout)

    # TEMPORARY?
    # Calculate alternative N?
    # cout[, N_ := n_eff * EST]  # TO DO?
    # all(cout$N_ <= cout$N)
    # plot(cout$N, cout$N_)
    # abline(0, 1, col = 2)

    # Retain only necessary variables
    #keep <- c('M', 'REP', bvars, 'ANALYSIS', 'level', 'n_eff', 'N', 'EST', 'SE')
    keep <- c('M', 'REP', bvars, 'ANALYSIS', 'level', 'n_eff', 'EST', 'SE')
    cout <- cout[, ..keep]

  } else {
    cout <- NULL
  }

  # Remove unnecessary columns in 'sim' before proceeding
  drop <- unique(avars[setdiff(acat, anum)])
  get_vars(sim, drop) <- NULL

  #----------
  #----------

  # Process the 'anum' analyses in a collap() call

  if (length(anum)) {

    cat("Computing estimates for numerical analyses:\n ~", paste(sapply(analyses[anum], rlang::f_text), collapse = "\n ~ "), "\n")

    # Variables in 'sim' needed for numerical analyses
    nvars <- unique(avars[anum])

    # Variables for which median is requested
    vmed <- unique(avars[afuns == "median"])

    # Restrict to necessary variables
    #sim <- get_vars(sim, c("M", nvars, names(blist)))

    # Set any 'Inf' values in analysis variables to NA
    for (v in nvars) {
      if (anyInf(sim[[v]])) {
        i <- whichv(sim[[v]], Inf)
        set(sim, i = i, j = v, value = NA)
        warning("Set ", length(i), " infinite values (", signif(100 * length(i) / nrow(sim), 3), "%) to NA for analysis variable ", v, " in 'sim'")
      }
    }

    # Create the generic grouping object for 'sim', used below to create 'nout'
    grp <- GRP(sim, by = c("M", "GRP_NAME", "GRP_VALUE"), sort = FALSE)

    # Sum of squared weights, by group
    # TO DO: Could be toggled between multithread and mclapply() but gains might be small
    w2sum <- lapply(nvars, function(v) {
      if (anyNA(sim[[v]])) {
        ok <- whichNA(sim[[v]], invert = TRUE)
        W <- ss(static, i = ind[ok], j = names(static), check = FALSE)
        g <- GRP(sim[ok], by = c("M", "GRP_NAME", "GRP_VALUE"), sort = FALSE)
      } else {
        W <- ss(static, i = ind, j = names(static), check = FALSE)
        g <- grp  # Uses the default grouping (no NA values)
      }
      fsum(x = W ^ 2, g = g, nthreads = cores) %>%
        add_vars(g$groups) %>%
        melt(id.vars = c('M', "GRP_NAME", "GRP_VALUE"), variable.name = "REP", value.name = "w2sum") %>%
        mutate(aname = factor(v),
               REP = as.integer(gsub("^REP__", "", REP)))
    }) %>%
      rbindlist()

    # Retain only the analysis variables; 'M' and 'by' variables not necessary since this information is stored in 'grp'
    get_vars(sim, grp$group.vars) <- NULL

    #---

    # Main calculation: Call qsu() -- and optionally fmedian() -- for each set of weights, in parallel
    nout <- parallel::mclapply(names(static), function(w) {

      # Get observations weights, matched to observations in 'sim'
      wgt <- ss(static, i = ind, j = w, check = FALSE)[[1]]

      # Call qsu() for variables
      out <- collapse::qsu(x = sim,
                           by = grp,
                           w = wgt,
                           #cols = vqsu,
                           labels = FALSE,
                           array = FALSE) %>%
        as.data.frame() %>%
        add_vars(grp$groups) %>%
        fmutate(REP = as.integer(gsub("^REP__", "", w)))

      # Returns data.table containing the median and its approximate density, by group 'g'
      median_density <- function(x, g, w, eps = 0.025) {
        p <- 0.5 + c(-eps, 0, eps)
        if (inherits(g, "GRP")) g <- g$group.id
        o <- collapse::radixorder(g, x)
        q <- sapply(p, function(n) collapse::fnth(x, n = n, g = g, w = w, o = o, use.g.names = FALSE, check.o = FALSE))  # Also accepts 'nthreads' argument
        f.med <- (p[3] - p[1]) / (q[, 3] - q[, 1]) # Approximate density at median
        return(data.table(median = q[, 2], density = f.med))
      }

      # Add median and approximate density, if requested
      if (length(vmed)) {
        setv(wgt, NA, 0)  # This is only necessary for 'fmedian' (will throw error or crash with missing weights)
        for (v in vmed) {
          med <- median_density(x = sim[[v]], g = grp, w = wgt) %>%
            setnames(paste0(v, c(".Median", ".Density")))
          out <- add_vars(out, med)
        }
      }

      # if (length(vmed)) {
      #   setv(wgt, NA, 0)  # This is only necessary for 'fmedian' (will throw error or crash with missing weights)
      #   med <- fmedian(x = get_vars(sim, vmed), g = grp, w = wgt) %>%
      #     setnames(paste0(vmed, ".Median"))
      #   out <- add_vars(out, med)
      # }

      return(out)

    }, mc.cores = cores) %>%
      rbindlist()

    # End of main calculation

    #---

    # Variable names list for subsequent melt() call
    mvars <- c("N", "WeightSum", "Mean", "SD", "Median", "Density") %>%
      lapply(function(x) paste(nvars, x, sep = ".")) %>%
      purrr::map(~ ifelse(.x %in% names(nout), .x, rep(NA, length(.x)))) %>%  # Handles case where median is requested for only some of the 'nvars'
      setNames(c('N', 'wsum', 'mean', 'stdev', 'median', 'density')) %>%
      purrr::discard(~ inherits(.x, "logical"))  # Drop 'median' entry if no median requested (i.e. all logical NA's)

    # Data frame giving the analyses/functions requested for each 'nvars'
    # The merge below expands result row-wise to make copies when/if multiple functions requested for the same 'aname' variable
    # This produces duplicate records when both mean and sum are requested (with identical data for the moment)
    # amerge <- afuns[anum] %>%
    #   enframe(name = "aname", value = "fn") %>%
    #   mutate(ANALYSIS = factor(paste(fn, aname, sep = ".")))
    amerge <- data.frame(aname = avars[anum], fn = afuns[anum]) %>%
      mutate(ANALYSIS = factor(paste(fn, aname, sep = ".")))

    # Post-processing to create final 'nout' object
    nout <- nout %>%
      melt(id.vars = c(grp$group.vars, "REP"), measure.vars = mvars, variable.name = "aname") %>%
      ftransform(aname = nvars[aname],
                 N = as.integer(N)) %>%  # qsu() returns double by default
      join(w2sum, on = c('M', 'REP', 'aname', 'GRP_NAME', 'GRP_VALUE'), verbose = FALSE, overid = 0) %>%
      join(amerge, on = "aname", multiple = TRUE, verbose = FALSE, overid = 0) %>%
      fmutate(
        level = NA,  # Placeholder for consistency with categorical output ('cout')
        n_eff = wsum ^ 2 / w2sum,
        se_mean = stdev / sqrt(n_eff),
        EST = fcase(fn == "mean", mean,
                    fn == "median", median,
                    fn == "sum", mean * wsum),
        SE = fcase(fn == "mean", se_mean,
                   fn == "median", fifelse(is.finite(density), (1 / (2 * density)) / sqrt(n_eff), se_mean * pi / 2),
                   fn == "sum", se_mean * wsum)) %>%
      #fsubset(!is.na(EST)) %>%
      widen_groups() %>%  # Make grouping variables wide
      get_vars(c('M', 'REP', bvars, 'ANALYSIS', 'level', 'N', 'n_eff', 'EST', 'SE')) # Restrict output to necessary variables

  } else {
    nout <- data.table()
  }

  #----------

  # Remove the principle data objects
  rm(sim, static, ind)

  # Combine analysis output data frames
  result <- rbind(nout, cout, fill = TRUE)
  rm(nout, cout)

  #----------

  # Data table alt:

  # Drop NA EST rows
  #result <- result[!is.na(EST)]

  # Calculate variables that must be summaried by 'group_vars'
  # This uses data.table for maximum speed
  result <- result[, list(
    n = .N,
    N_eff = mean(n_eff),
    est = mean(EST),
    ubar = mean(SE ^ 2),
    b = var(EST),  # Returns NA when .N = 1
    #b = ifelse(.N == 1, 0, var(EST)),  # Returns 0 when .N = 1
    term2_sum = sum((SE ^ 2) ^ 2)
  ), by = c(bvars, "level", "ANALYSIS")]

  # Add final output variables
  # This uses mutate() to allow for sequential evaluation of the variables

  result <- result %>%
    mutate(
      se = sqrt(ubar + (1 + 1 / n) * b),
      #var = ubar + (1 + 1 / n) * b,
      term1 = (1 + (ubar / b)) ^ 2,
      term2 = (1 / (n - 1)) + (1 / (n ^ 2)) * term2_sum / b ^ 2,
      #term2 = (1 / (n - 1)) + (1 / (n ^ 2)) * sum((SE ^ 2) ^ 2) / b ^ 2
      df = (n - 1) * term1 / term2,
      moe = se * suppressWarnings(qt(p = 0.95, df)),
      cv = 100 * (moe / 1.645) / est    # Coefficient of variation (https://sites.tufts.edu/gis/files/2013/11/Amercian-Community-Survey_Margin-of-error-tutorial.pdf)
    ) %>%

    #----------

  # Compute final estimate and MOE across all M*(R + 1) samples
  # Follows methodology here: https://ummel.github.io/fusionACS/methodology.html
  # NOTE: This does NOT implement the ACS-based SDR replicates approach
  # TO DO: Enable separate calculation using SDR approach -- it only required estimates (not intermediary SE's)

  # Compute final estimates
  # result <- result %>%
  #   #fsubset(!is.na(EST)) %>%  # Necessary in case an estimate was impossible for a particular sub-group
  #   group_by_at(c(bvars, "level", "ANALYSIS")) %>%
  #
  #   # Summarize estimate and variance across samples
  #   summarize(
  #
  #     # Total number of samples
  #     n = n(),
  #
  #     # Mean number of microdata observations per sample
  #     #N = mean(N),
  #
  #     # Mean effective sample size per sample
  #     N_eff = mean(n_eff),
  #
  #     # Compute the central estimate, across samples
  #     est = mean(EST),
  #
  #     # Compute the variance of the estimates, across samples
  #     ubar = mean(SE ^ 2),
  #     #b = ifelse(n == 1, 0, var(EST)),
  #     b = var(EST),
  #     var = ubar + (1 + 1 / n) * b,
  #
  #     # Standard error
  #     #se = sqrt(var),
  #
  #     # Rubin's 'r' used in calculation of degrees of freedom
  #     # Set to Inf if ubar = 0 (would return NA otherwise)
  #     #r = ifelse(ubar == 0, Inf, (1 + 1 / n) * b / ubar),
  #
  #     # Degrees of freedom assuming infinite population (original Rubin 1987 degrees of freedom)
  #     # Confirmation of 'df' in case of replicate weights only: https://stats.stackexchange.com/questions/380467/degrees-of-freedom-for-successive-differences-weights
  #     #df = ifelse(n == 1, Inf, (n - 1) * (1 + r^(-1)) ^ 2),
  #
  #     # Barnard and Rubin (1999) degrees of freedom
  #     # Note that this is placed inside the grouped summarize() calculation due to the summation in 'term2'
  #     term1 = (1 + (ubar / b)) ^ 2,
  #     term2 = (1 / (n - 1)) + (1 / (n ^ 2)) * sum((SE ^ 2) ^ 2) / b ^ 2,
  #     df = (n - 1) * term1 / term2,
  #
  #     # Final 90% confidence interval (p = 0.95 means a 90% confidence interval)
  #     # Based on the PUMS "User Verification" estimates provided by Census Bureau, it appears they use a t-score of 1.645 when using replicate weights,
  #     #  but using the 'df' calculation (above) is more correct as explained here: https://stats.stackexchange.com/questions/380467/degrees-of-freedom-for-successive-differences-weights
  #     #moe = se * suppressWarnings(qt(p = 0.95, df)),
  #
  #     # Coefficient of variation (https://sites.tufts.edu/gis/files/2013/11/Amercian-Community-Survey_Margin-of-error-tutorial.pdf)
  #     #cv = 100 * (moe / 1.645) / abs(est),
  #
  #     .groups = "drop"
  #
  #   ) %>%
  #
  #   # Estimates and associated results are suppressed under two conditions:
  #   # 1. There are less than 3 valid unweighted microdata observations (per estimate); for proportions, number of observations must be at least 3x the number of levels
  #   # 2. MOE is requested (R > 0) but only a single valid estimate is available across the replicates (Rn == 1); can occur due to missingness.
  #   # See Table 2: https://www2.census.gov/programs-surveys/acs/tech_docs/data_suppression/ACS_Data_Release_Rules.pdf
  #   group_by_at(c(bvars, "ANALYSIS")) %>%
  #   #mutate(suppress = (N_eff < 3 * n()) | (R > 0 & n == 1)) %>%
  #   #mutate(suppress = (N_eff < 3 * n())) %>%
  #   mutate(suppress = FALSE) %>%
  #   ungroup() %>%
  #
  #   mutate(
  #
  #     # Suppress the estimate if the number of unweighted cases is too low
  #     est = ifelse(suppress, NA, est),
  #
  #     # Final standard error (Rubin)
  #     # Ensure variance set to NA if multiple replicates not available
  #     se = ifelse(suppress | is.na(est), NA, sqrt(var)),
  #
  #     # Uncertainty estimation suppressed (NA) if multiple weights are not present
  #     #se = ifelse(n == 1, NA, se),
  #
  #     # Rubin's 'r' used in calculation of degrees of freedom
  #     # Set to Inf if ubar = 0 (would return NA otherwise)
  #     #r = ifelse(ubar == 0, Inf, (1 + n^(-1)) * b / ubar),
  #
  #     # Degrees of freedom assuming infinite population (original Rubin 1987 degrees of freedom)
  #     # Confirmation of 'df' in case of replicate weights only: https://stats.stackexchange.com/questions/380467/degrees-of-freedom-for-successive-differences-weights
  #     #df = if (Mimp == 1) {pmax(1, length(wvars) - 2L)} else  {(Mimp - 1) * (1 + r^(-1)) ^ 2},
  #     #df = if (Mimp == 1 & !use.up) R - 1L else (Mn - 1) * (1 + r^(-1)) ^ 2,
  #     #df = ifelse(r == 0, Rn - 1, df),  # If 'r' is zero, there is no variance across the implicates, so we derive 'df' from the number of weights only
  #     #df = ifelse(n == 1, Inf, (n - 1) * (1 + r^(-1)) ^ 2),
  #     #df = ifelse(suppress, NA, df),
  #
  #     # Final 90% confidence interval (p = 0.95 means a 90% confidence interval)
  #     # Based on the PUMS "User Verification" estimates provided by Census Bureau, it appears they use a t-score of 1.645 when using replicate weights,
  #     #  but using the 'df' calculation (above) is more correct as explained here: https://stats.stackexchange.com/questions/380467/degrees-of-freedom-for-successive-differences-weights
  #     moe = se * suppressWarnings(qt(p = 0.95, df)),
  #
  #     # Coefficient of variation (https://sites.tufts.edu/gis/files/2013/11/Amercian-Community-Survey_Margin-of-error-tutorial.pdf)
  #     cv = 100 * (moe / 1.645) / est
  #
  #   ) %>%

  mutate(i = fmatch(ANALYSIS, paste(afuns, avars, sep = "."), overid = 0), # Returns the analysis number (index); possibly multiple, associated with each ANALYSIS
         lhs = alhs[i],
         rhs = arhs[i],
         type = afuns[i], # Determine the type of analytical result returned
         type = ifelse(!is.na(level) & type == "mean",  "prop", type),
         type = ifelse(!is.na(level) & type == "sum",  "count", type)) %>%

    # Arranges row order of output
    arrange(i, !!!rlang::syms(bvars), level) %>%

    # Replace NaN from zero division with normal NA
    mutate_all(tidyr::replace_na, replace = NA) %>%

    # Convert data types, preserving leading zeros in strings (e.g. '012345')
    # Useful for character grouping variables that can be typed as integer, logical, etc.
    mutate_all(~ if (any(grepl("^0[0-9]+$", .x) & .x != "0")) {as.character(.x)} else {type.convert(.x, as.is = TRUE)}) %>%

    # Clean up precision and integer type
    mutate_if(is.double, convertInteger, threshold = 1) %>%
    mutate_if(is.double, signif, digits = 5) %>%

    # Select final variables
    #select(lhs, rhs, type, all_of(bvars), level, N_eff, est, moe, se, df, cv) %>%
    select(lhs, rhs, type, all_of(bvars), level, N_eff, ubar, b, est, moe, se, df, cv) %>%

    # !!!!TEMPORARY - return only subset of results
    select(lhs, rhs, type, all_of(bvars), level, N_eff, est, moe, cv) %>%

    # setattr(name = "var_source", value = var.sources) %>%
    # setattr(name = "analyze_call", value = mcall) %>%
    # setattr(name = "analyze_fun", value = fun)

    # Return tibble instead of data.table
    as_tibble()


  return(result)

}

#-------

collect_formulas <- function(...) {
  args <- enquos(...)
  formulas <- list()

  for (i in seq_along(args)) {
    q <- args[[i]]
    name <- names(args)[i]
    expr <- get_expr(q)

    # Case 1: Formula
    if (is_formula(expr)) {
      lhs <- f_lhs(expr)
      rhs <- f_rhs(expr)

      # Add default LHS if missing
      if (is.null(lhs)) {
        # Try to infer function and variable name from RHS
        if (is.call(rhs) && length(rhs) >= 2) {
          fun_name <- as_string(rhs[[1]])
          var_name <- as_string(rhs[[2]])
          lhs <- sym(paste0(fun_name, "_", var_name))
        } else {
          stop("Can't construct default LHS for malformed formula: ", expr_label(expr))
        }
      }

      formulas[[length(formulas) + 1]] <- new_formula(lhs, rhs)

      # Case 2: Named argument like mean = c(x1, x2) or mean = c("x3", "x4")
    } else if (!is.null(name) && nzchar(name)) {
      if (is.call(expr) && identical(expr[[1]], as.name("c"))) {
        values <- as.list(expr)[-1]
      } else {
        values <- list(expr)
      }

      for (v in values) {
        var_name <- as_string(v)
        lhs <- sym(paste0(name, "_", var_name))
        rhs <- call2(name, sym(var_name))
        formulas[[length(formulas) + 1]] <- new_formula(lhs, rhs)
      }

    } else {
      stop("Unsupported input: must be a formula or a named vector like mean = c(x, y) or mean = c('x', 'y')")
    }
  }

  return(formulas)
}

# TESTING
# collect_formulas(
#   ~mean(x1),
#   myalias ~ median(x2),
#   mean = c(x3, x4),
#   median = c("x5", "x6")
# )

#-------------------------------

parse_by <- function(expr) {
  # Handle NULL
  if (is.null(expr)) {
    return(list())
  }

  # Normalize: turn symbol, string, or c(...) into list(...)
  if (is.symbol(expr) || is.character(expr) ||
      (is.call(expr) && identical(expr[[1]], as.name("c")))) {
    expr <- as.call(c(quote(list), expr))
  }

  if (!is.call(expr) || !identical(expr[[1]], as.name("list"))) {
    stop("Unsupported input format")
  }

  extract <- function(x) {
    if (is.symbol(x)) {
      as.character(x)
    } else if (is.character(x)) {
      x
    } else if (is.call(x) && identical(x[[1]], as.name("c"))) {
      vapply(x[-1], extract, character(1))
    } else {
      stop("Unsupported item: must be a symbol or string")
    }
  }

  lapply(expr[-1], extract)
}

#---

widen_groups <- function(dt) {

  dt[, .GRPID := .I] # This is necessa
  #---------------ry to correctly dcast() in cases where groups happen to have identical values on other non-grouping variables (e.g. if two groups are identical sub-populations but labelled differently)
  dt <- dcast(dt, formula = ... ~ GRP_NAME, value.var = "GRP_VALUE")

  # Process any composite (more than one variable) by-group columns to reconstitute the original, separated columns
  for (col in grep("__", names(dt), value = TRUE)) {

    # Split column name into new variable names
    new_vars <- strsplit(col, "__")[[1]]

    # Split values into separate columns
    split_vals <- tstrsplit(dt[[col]], "__", fixed = TRUE)

    for (i in seq_along(new_vars)) {
      var_name <- new_vars[i]
      new_vals <- split_vals[[i]]
      if (var_name %in% names(dt)) {
        # Update only where original value is NA and split value is not NA
        idx <- is.na(dt[[var_name]]) & !is.na(new_vals)
        dt[idx, (var_name) := new_vals[idx]]
      } else {
        # Create new column
        dt[, (var_name) := new_vals]
      }
    }

    # Remove the composite column
    dt[, (col) := NULL]
  }

  return(dt)

}
