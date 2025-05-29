# Specify package imports
#' @import stringr
#' @import dplyr
#' @import ellmer
#' @import arrow
#' @import collapse
#' @importFrom stats density df median qt setNames var
#' @rawNamespace import(data.table, except = c(first, last, between, fdroplevels))
#' @rawNamespace import(rlang, except = c("string", ":="))
NULL

# attachment::att_amend_desc()

# Utility/helper functions

# Return suitable number of cores for use by arrow
get_cores <- function() {
  cores <- parallel::detectCores(logical = FALSE)
  if (is.na(cores)) cores <- parallel::detectCores(logical = TRUE)
  return(cores - 1L)
}

# Quickly identify if any values are Infinite; complement to anyNA()
# Note that Inf is always double, so an integer vector cannot have Inf (if would need to be coerved to double)
# See here: https://stackoverflow.com/questions/39849650/why-typeofinf-is-double
anyInf <- function(x) {
  if (is.double(x)) collapse::anyv(x, Inf) else FALSE
}

# Function to convert a numeric vector to integer, if possible
# Checks if maximum value is coercible to 32-bit integer; see ?integer "Details"
# If the fraction of integer-coercible values exceeds 'threshold', then non-integer values are coerced to integer
convertInteger <- function(x, threshold = 0.99) {
  if (collapse::allNA(x)) {
    x <- as.logical(x)
  } else {
    ok32 <- max(x, na.rm = TRUE) <= .Machine$integer.max
    if (ok32) {
      chk <- collapse::na_rm(x) %% 1 == 0
      if (sum(chk) / length(chk) >= threshold) {
        x <- as.integer(round(x))
      }
    }
  }
  return(x)
}
