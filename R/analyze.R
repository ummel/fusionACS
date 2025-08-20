#' Analyze fusionACS microdata
#'
#' @description
#' Analyze a fusionACS microdata pseudo-sample created by \link{assemble}. Efficiently computes means, medians, sums, proportions, and counts of specified variables, optionally across population subgroups.
#'
#' @param data Data frame. fusionACS microdata pseudo-sample returned by \link{assemble}.
#' @param ... Formulas. Used to define the desired analyses. See Examples.
#' @param by Optional variable(s) that collectively define the set of population subgroups for which each analysis is computed. Can be a mix of geographic (e.g. census tract) and/or socio-demographic microdata variables (e.g. poverty status); the latter may be existing variables on disk or custom variables created on-the-fly via \code{fun()}. If \code{NULL}, analysis is done for the whole (national) sample.
#' @param fun Function. Optional function for modifying \code{data} prior to analysis. \code{fun(data)} is called internally and the requested analyses performed on the output.
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

# source("data/utils.R")
# data <- assemble(variables = c(hincp, np, cookfuel, hotma, dollarel, state_name, county10), year = 2015:2019, respondent = 'household', state_name == "Texas")
# analyses <- collect_formulas(~ mean(hotma), ~mean(cookfuel), ~mean(dollarel), ~median(dollarel))
# by = list('np', c('np', 'county10'))
# fun = NULL
# cores = ncores

#--------

analyze <- function(data,
                    ...,
                    by = NULL,
                    fun = NULL,
                    cores = get_cores()

) {

  collapse::set_collapse(nthreads = cores)
  data.table::setDTthreads(cores)
  options(datatable.showProgress = FALSE)

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
  #options(collapse_unused_arg_action = "none")

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

  # Capture the analysis formulas from ... argument
  analyses <- collect_formulas(...)

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

  # An analysis variable cannot be in 'by'
  err <- intersect(avars, bvars)
  if (length(err)) stop("Analysis variables cannot also be in the 'by' argument: ", paste(err, collapse = ", "))

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
  miss <- setdiff(c('M', 'weight', avars, bvars), names(data))  # Check for missing required variables, including 'by' variables
  if (length(miss)) {
    if (is.null(fun)) {
      stop("The following required variables are missing from 'data':\n", paste(miss, collapse = "\n"))
    } else {
      stop("The supplied 'fun' does not return the following required variables:\n", paste(miss, collapse = "\n"))
    }
  }

  # Success message
  if (!is.null(fun)) cat("Successfully applied user 'fun' to 'data'\n")

  # Restrict 'data' to only the necessary variables
  drop <- setdiff(names(data), c('M', 'weight', avars, bvars))
  get_vars(data, drop) <- NULL

  #---

  # Issue a warning if any of the 'by' variables have >100k unique values
  if (length(bvars)) {
    n <- sapply(get_vars(data, bvars), uniqueN, na.rm = TRUE)
    if (any(n > 100e3)) {
      warning("The following 'by' variables have greater than 100,000 unique values (is this really what you want?): ", bvars[n > 100e3], immediate. = FALSE)
    }
  }

  # Determine which analyses are categorical and have outer function "mean" or "sum"
  acat <- which(avars %in% cat_vars(data, return = "names"))
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
      set(data, j = names(blist)[i], value = collapse::finteraction(get_vars(data, blist[[i]]), sort = FALSE, sep = "__"))
    }
  }

  # Remove 'by' variables no longer needed because they are (only) part of a combination variable
  if (length(bvars)) {
    get_vars(data, setdiff(bvars, names(blist))) <- NULL
  }

  #---

  # Do categorical analysis....

  # Do the categorical variable calculations
  if (length(acat)) {

    cat("Computing estimates for categorical analyses:\n ~", paste(sapply(analyses[acat], rlang::f_text), collapse = "\n ~ "), "\n")

    # Variables in 'sim' needed for categorical analyses
    cvars <- unique(avars[acat])

    # The initial calculation
    grp <- GRP(data, by = c('M', names(blist), cvars), sort = FALSE)

    # Columns to drop because no longer needed after initial 'grp' formation
    drop <- if (length(anum)) setdiff(cvars, avars[anum]) else grp$group.vars
    get_vars(data, drop) <- NULL

    #---

    # Initial summation by group
    # This doesn't provide final summation, but it greatly reduces the number of rows without requiring any data transformation
    d <- add_vars(grp$groups, data[, .(weight)] %>%
                    frename(W = weight) %>%
                    fmutate(W2 = W ^ 2) %>%
                    fsum(g = grp, nthreads = cores))

    # just for reference/testing to see the dimensionality reduction
    #nrow(d) / nrow(data)

    #---

    # NECESSSARY?
    # Coerce 'by' variables to character to avoid any variable type conflicts during melt
    # For example, if the 'by' variables mix integer and factor
    #d[, (names(blist)) := lapply(.SD, as.character), .SDcols = names(blist)]

    # Melt by grouping variables ('blist')
    if (length(bvars)) {
      d[, (names(blist)) := lapply(.SD, as.character), .SDcols = names(blist)]
      d <- melt(d, measure.vars = names(blist), variable.name = "GRP_NAME", value.name = "GRP_VALUE", na.rm = TRUE)  # data.table::melt()
      #d <- collapse::pivot(d, values = names(blist), names = list("GRP_NAME", "GRP_VALUE"), na.rm = TRUE)  # collapse::pivot(); slightly faster but buggy?
      if (is.character(d$GRP_VALUE)) d[, GRP_VALUE := as.factor(GRP_VALUE)]
    } else {
      # Placeholders if no 'by' argument specified
      d[, `:=`(GRP_NAME = 1L, GRP_VALUE = 1L)]
    }

    #---

    # Melt by categorical analysis variables ('cvars')
    d[, (cvars) := lapply(.SD, as.character), .SDcols = cvars]
    d <- melt(d, measure.vars = cvars, variable.name = "VAR_NAME", value.name = "VAR_LEVEL", na.rm = TRUE)
    if (is.character(d$VAR_LEVEL)) d[, VAR_LEVEL := as.factor(VAR_LEVEL)]

    # Melt by 'cvars' using collapse::pivot()
    # collapse::pivot(); slightly faster but buggy?
    #d <- pivot(d, values = cvars, names = list("VAR_NAME", "VAR_LEVEL"), na.rm = TRUE)

    grp <- GRP(d, by = c("M", "GRP_NAME", "GRP_VALUE", "VAR_NAME", "VAR_LEVEL"), sort = FALSE)
    get_vars(d, grp$group.vars) <- NULL

    # Sum W and W2, by group, in parallel
    d <- add_vars(grp$groups, fsum(x = d, g = grp, nthreads = cores))

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
    u1 <- d %>%
      get_vars(c("M", "GRP_NAME", "GRP_VALUE")) %>%
      funique()
    u2 <- d %>%
      get_vars(c("VAR_NAME", "VAR_LEVEL")) %>%
      funique()
    temp <- CJ.dt(u1, u2)
    temp <- fsetdiff(temp, get_vars(d, names(temp)))
    if (nrow(temp) > 0) {
      temp <- add_vars(temp,
                       W = alloc(0, nrow(temp)),
                       W2 = alloc(0, nrow(temp)))
      d <- rbind(d, temp, fill = TRUE)
      rm(temp)
    }

    #---

    # Expand results row-wise to make copies when multiple functions requested for the same 'aname' variable
    # The merge below expands result row-wise to make copies when/if multiple functions are requested for the same analysis variable
    # This produces duplicate records when both proportion and sum are requested (with identical data for the moment; sums are adjusted below)
    amerge <- lapply(cvars, function(v) {
      data.frame(VAR_NAME = v, ANALYSIS = afuns[avars == v]) %>%
        mutate(ANALYSIS = factor(paste(ANALYSIS, VAR_NAME, sep = ".")))
    }) %>%
      rbindlist()

    d <- join(d, amerge, on = "VAR_NAME", multiple = TRUE, verbose = FALSE, overid = 0)

    #---

    # Final calculation
    grp <- GRP(d, by = c("M", "GRP_NAME", "GRP_VALUE", "VAR_NAME"), sort = FALSE)
    add_vars(d) <- fselect(d, W, W2) %>%
      fsum(g = grp, TRA = "replace", nthreads = cores) %>%
      frename(Wsum = W, W2sum = W2)

    b <- copy(d)

    # Calculate estimate and effective sample size
    # This correctly returns NA's if the group in question has no observations (i.e. Wsum == 0); such cases are dropped below
    d[, `:=`(EST = W / Wsum,
             n_eff = Wsum ^ 2 / W2sum)]

    # Calculate the Agresti-Coull adjusted point estimate, assuming a 90% confidence interval (z = 1.644854)
    z <- 1.644854
    d[, EST_ac := (EST * n_eff + z ^ 2 / 2) / (n_eff + z ^ 2)]

    # Calculate standard error of proportions using the Agresti-Coull adjusted point estimates and effective sample size
    d[, SE := sqrt(EST_ac * (1 - EST_ac) / n_eff)]

    # For requested sums (i.e. counts), calculate the estimate (total weight) and standard error
    j <- grepl("^sum", d$ANALYSIS)
    d[j, EST := W]
    d[j, SE := SE * Wsum]

    cout <- d %>%
      fselect(M, ANALYSIS, VAR_LEVEL, n_eff, EST, SE, GRP_NAME, GRP_VALUE) %>%
      fsubset(is.finite(EST) & is.finite(SE)) %>%  # Remove infeasible results, typically due to very small sample size
      frename(level = VAR_LEVEL)

  } else {
    cout <- NULL
  }

  #---

  # Process the 'anum' analyses in a collap() call

  if (length(anum)) {

    cat("Computing estimates for numerical analyses:\n ~", paste(sapply(analyses[anum], rlang::f_text), collapse = "\n ~ "), "\n")

    # Variables in 'sim' needed for numerical analyses
    nvars <- unique(avars[anum])

    # Variables for which median is requested
    vmed <- unique(avars[afuns == "median"])

    # Restrict to necessary variables
    d <- get_vars(data, c("M", 'weight', names(blist), nvars))
    rm(data)

    #---

    # NECESSSARY?
    # Coerce 'by' variables to character to avoid any variable type conflicts during melt
    # For example, if the 'by' variables mix integer and factor
    #d[, (names(blist)) := lapply(.SD, as.character), .SDcols = names(blist)]

    # Melt by group variables
    if (length(bvars)) {
      d[, (names(blist)) := lapply(.SD, as.character), .SDcols = names(blist)]
      #d <- melt(d, measure.vars = names(blist), variable.name = "GRP_NAME", value.name = "GRP_VALUE", na.rm = TRUE)  # data.table::melt()
      d <- collapse::pivot(d, values = names(blist), names = list("GRP_NAME", "GRP_VALUE"), na.rm = TRUE)  # collapse::pivot(); slightly faster but buggy?
      if (is.character(d$GRP_VALUE)) d[, GRP_VALUE := as.factor(GRP_VALUE)]
    } else {
      # Placeholders if no 'by' argument specified
      d[, `:=`(GRP_NAME = 1L, GRP_VALUE = 1L)]
    }

    #---

    # Create the generic grouping object for 'sim', used below to create 'nout'
    grp <- GRP(d, by = c("M", "GRP_NAME", "GRP_VALUE"), sort = FALSE)

    # Columns to drop because no longer needed after initial 'grp' formation
    get_vars(d, grp$group.vars) <- NULL

    b <- copy(d)

    # Get the sum of squared weights
    # NOTE: This is not maximally efficient when there are no NA values in 'nvars'
    nav <- sapply(nvars, function(v) anyNA(d[[v]]))
    w2sum <- grp$groups
    if (any(nav)) {
      temp <- data.table()
      for (v in names(which(nav))) set(temp, j = v, value = d$weight * copyv(d[[v]], NA, 1, invert = TRUE))
      w2sum <- add_vars(w2sum, fsum(x = temp, g = grp, w = d$weight, nthreads = cores))
    }
    if (any(!nav)) {
      temp <- fsum(x = d$weight, g = grp, w = d$weight)  # This is the correct result for any 'nvars' without NA values
      for (v in names(which(!nav))) set(w2sum, j = v, value = temp)
    }
    w2sum <- melt(w2sum, measure.vars = nvars, variable.name = "aname", value.name = "w2sum")
    rm(temp)

    #---

    # Initial qsu() by group
    # Note that qsu() correctly returns sum of weights already adjusted for NA values in 'nvars'
    nout <- collapse::qsu(x = d,
                          by = grp,
                          w = ~ weight,
                          cols = nvars,
                          labels = FALSE,
                          array = FALSE) %>%
      as.data.frame() %>%
      add_vars(grp$groups)

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
    # NOTE: Could this be done in parallel?
    if (length(vmed)) {
      if (anyNA(d$weight)) d[, weight := setv(weight, NA, 0)] # setv() only necessary for 'fmedian' (will throw error or crash with missing wei
      for (v in vmed) {  # <-- parallelize the loop?
        med <- median_density(x = d[[v]], g = grp, w = d$weight) %>%
          setnames(paste0(v, c(".Median", ".Density")))
        nout <- add_vars(nout, med)
      }
    }

    rm(d)

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
    amerge <- data.frame(aname = avars[anum], fn = afuns[anum]) %>%
      mutate(ANALYSIS = factor(paste(fn, aname, sep = ".")))

    # Post-processing to create final 'nout' object
    nout <- nout %>%
      qDT() %>%
      melt(id.vars = grp$group.vars, measure.vars = mvars, variable.name = "aname") %>%
      ftransform(aname = nvars[aname],
                 N = as.integer(N)) %>%  # qsu() returns double by default
      join(w2sum, on = c('M', 'GRP_NAME', 'GRP_VALUE', 'aname'), verbose = FALSE, overid = 0) %>%
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
      fsubset(is.finite(EST) & is.finite(SE)) %>%  # Remove infeasible results, typically due to very small sample size
      fselect(M, ANALYSIS, level, n_eff, EST, SE, GRP_NAME, GRP_VALUE)

  } else {
    nout <- data.table()
  }

  #---

  # Remove the principle data objects
  #rm(sim, static, ind)

  # Combine analysis output data frames
  result <- rbind(nout, cout, fill = TRUE) %>%
    widen_groups()

  #rm(nout, cout)

  #----------

  cat("Computing final point estimates and margin of error\n")

  # Calculate variables that must be summarized by 'group_vars'
  # This uses data.table for maximum speed
  result <- result[, list(
    n = .N,
    N_eff = mean(n_eff),
    est = mean(EST),
    ubar = mean(SE ^ 2),
    #b = var(EST),  # Returns NA when .N = 1
    b = ifelse(.N == 1, 0, var(EST)),  # Returns 0 when .N = 1, which allows 'se' to calculate even if only single implicate
    term2_sum = sum((SE ^ 2) ^ 2)
  ), by = c(bvars, "level", "ANALYSIS")]

  # Add final output variables
  # This uses mutate() to allow for sequential evaluation of the variables

  result <- result %>%
    mutate(
      se = sqrt(ubar + (1 + 1 / n) * b),
      term1 = (1 + (ubar / b)) ^ 2,
      term2 = (1 / (n - 1)) + (1 / (n ^ 2)) * term2_sum / b ^ 2,
      df = (n - 1) * term1 / term2,
      df = fifelse(!is.finite(df), N_eff - 1, df), # Fallback for cases where 'df' cannot be calculated (usually because b = 0)
      moe = se * suppressWarnings(qt(p = 0.95, df)),
      est = fifelse(!is.finite(moe), NA, est),  # Fallback to set estimate to NA if the 'moe' cannot be calculated (usually because N_eff <= 1)
      cv = 100 * (moe / 1.645) / est    # Coefficient of variation (https://sites.tufts.edu/gis/files/2013/11/Amercian-Community-Survey_Margin-of-error-tutorial.pdf)
    ) %>%

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

    # NOTE: This is not safe for tract, for example, since it converts to double
    # TO DO: Enforce original input type in the output!!!
    # Convert data types, preserving leading zeros in strings (e.g. '012345')
    # Useful for character grouping variables that can be typed as integer, logical, etc.
    #mutate_all(~ if (any(grepl("^0[0-9]+$", .x) & .x != "0")) {as.character(.x)} else {type.convert(.x, as.is = TRUE)}) %>%

    # Clean up precision and integer type
    mutate_if(is.double, convertInteger, threshold = 1) %>%
    mutate_if(is.double, signif, digits = 5) %>%

    # Select final variables
    #select(lhs, rhs, type, all_of(bvars), level, N_eff, est, moe, se, df, cv) %>%
    #select(lhs, rhs, type, all_of(bvars), level, N_eff, ubar, b, est, moe, se, df, cv) %>%

    # Select final variables
    select(lhs, rhs, type, all_of(bvars), level, N_eff, est, moe) %>%

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

  nms <- copy(names(dt))
  dt[, .GRPID := .I]  # This is necessary to correctly dcast() in cases where groups happen to have identical values on other non-grouping variables (e.g. if two groups are identical sub-populations but labelled differently)
  keep <- setdiff(names(dt), c('GRP_NAME', 'GRP_VALUE'))
  temp <- dt[, ..keep]
  dt <- dcast(dt[, .(.GRPID, GRP_NAME, GRP_VALUE)], formula = ... ~ GRP_NAME, value.var = "GRP_VALUE")

  # Process any composite (more than one variable) by-group columns to reconstitute the original, separated columns
  for (col in grep("__", names(dt), value = TRUE)) {

    # Split column name into new variable names
    new_vars <- strsplit(col, "__")[[1]]

    # Split values into separate columns
    split_vals <- data.table::tstrsplit(dt[[col]], "__", fixed = TRUE, type.convert = FALSE)

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

  # Join original columns
  dt <- dt[temp, on = ".GRPID"]
  dt[, .GRPID := NULL]
  setcolorder(dt, intersect(nms, names(dt)))
  return(dt)

}
