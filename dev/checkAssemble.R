#' Check if a proposed microdata assembly is valid
#'
#' This function takes proposed inputs to assemble() -- variables and year -- and determines if the inputs are valid given the available microdata.
#'
#' @param variables Character vector specifying the names of survey variables.
#' @param year Numeric vector specifying the year(s) of ACS-PUMS microdata to use.
#' @param respondent Character. Whether to return "household" or "person" microdata (i.e. the type of survey respondent).
#' @return If the proposed inputs are valid, returns a list with named slots "variables" and "year".
#' @return If the proposed inputs are NOT valid, returns a character string indicating what is wrong.

#' @export

#-----

# variables = c("btuel", "hincp", "puma20")
# year = 2015:2022
# respondent = "household"

checkAssemble <- function(variables, year, respondent) {

  d <- dictionary()

  # If there are any list columns, convert to text
  for (i in which(sapply(d, inherits, what = "list"))) {
    data.table::set(d, j = i, value = sapply(d[[i]], paste, collapse = " "))
  }

  check <- d %>%
    mutate(ok_years = rowSums(!sapply(year, function(x) grepl(x, d$acs_years))) == 0) %>%
    filter(ok_years, variable %in% !!variables)
           #respondent == !!respondent)

  # Check if all of the 'variables' pass the check
  miss <- setdiff(variables, check$variable)
  out <- if (length(miss) > 0) {
    paste("Error. The following 'variables' are not available given the specified 'year' argument:", paste(miss, collapse = ", "))
  } else {
    list(variables = variables, year = year, respondent = respondent)
  }

  return(out)

}
