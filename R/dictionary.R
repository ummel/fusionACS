#' Load fusionACS microdata dictionary
#'
#' Returns a data frame containing metadata for the microdata variables in the local fusionACS database.
#'
#' @param directory Path to the local fusionACS data directory.
#' @return A data frame with the following columns:
#' \describe{
#'   \item{variable}{Variable name (column in microdata).}
#'   \item{description}{Variable description or definition.}
#'   \item{survey}{Acronym of the survey where the variable comes from.}
#'   \item{vintage}{Vintage of the donor survey where the variable comes from (NA for ACS variables; see 'years' instead).}
#'   \item{respondent}{Does the variable apply to "household" or "person" microdata observations?}
#'   \item{type}{The data type of the variable (e.g. integer).}
#'   \item{n_values}{Number of unique values the variable exhibits.}
#'   \item{values}{Summary of the values found in the microdata. For numeric variables, returns the output of \code{\link[base]{summary}}. For categorical variables, returns a list of the unique values/levels.}
#'   \item{years}{Year(s) of ACS-PUMS microdata for which the variable is available.}
#'   \item{custom}{Logical indicating a 'custom' ACS variable created by the fusionACS team. See: https://github.com/ummel/fusionData/tree/master/survey-processed/ACS/custom}
#' }
#' @examples
#' dict <- dictionary()
#' View(dict)
#' @export

dictionary <- function(directory = get_directory(), verbose = TRUE) {

  p <- try(normalizePath(directory, winslash = "/", mustWork = TRUE), silent = TRUE)
  if (inherits(p, "try-error")) stop("Could not resolve the 'directory'")
  d <- read_parquet(file.path(p, "dictionary.parquet"))
  d <- select(d, -file)  # Remove 'file' column since it isn't useful to the user

  # Report dictionary summary, if requested
  if (verbose) {
    g <- d %>% filter(survey == "geography")
    s <- d %>% filter(survey != "geography")
    cli_alert_info(
      paste(
        "There are", nrow(s), "variables available across", uniqueN(s$survey), "surveys:\n",
        paste(unique(s$survey), collapse = ", "),
        "\nAs well as", nrow(g), "geographic variables. See ?dictionary for details."
      )
    )
  }

  # Return data frame invisibly (it must be assigned by user)
  return(invisible(d))

}
