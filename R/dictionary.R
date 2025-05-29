#' Load fusionACS microdata dictionary
#'
#' Returns a data frame containing metadata for the microdata variables in the local fusionACS database.
#'
#' @param directory Path to the local fusionACS data directory.
#' @return A data frame with the following columns:
#' \describe{
#'   \item{variable}{Variable name (column in microdata).}
#'   \item{description}{Variable description or definition.}
#'   \item{source_survey}{Acronym of the survey where the variable comes from.}
#'   \item{source_vintage}{Vintage of the survey where the variable comes from.}
#'   \item{respondent}{Does the variable apply to "household" or "person" microdata observations?}
#'   \item{acs_years}{Year(s) of ACS-PUMS microdata for which the variable is available.}
#'   \item{type}{The data type of the variable (e.g. integer).}
#'   \item{n_unique}{Number of unique values observed in the microdata.}
#'   \item{values}{Summary of the values found in the microdata. For numeric variables, returns the output of \code{\link[base]{summary}}. For categorical variables, returns the unique values/levels (each enclosed in '<>').}
#' }
#' @examples
#' dict <- dictionary()
#' View(dict)
#' @export

dictionary <- function(directory = get_directory()) {
  p <- try(normalizePath(directory, winslash = "/", mustWork = TRUE), silent = TRUE)
  if (inherits(p, "try-error")) stop("Could not resolve the 'directory'")
  d <- read_parquet(file.path(p, "dictionary.parquet"))
  return(d)
}
