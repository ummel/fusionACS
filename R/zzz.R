# Specify package imports
#' @import cli
#' @import stringr
#' @import dplyr
#' @import arrow
#' @import collapse
#' @importFrom stats density df median qt setNames var
#' @rawNamespace import(data.table, except = c(first, last, between, fdroplevels))
#' @rawNamespace import(rlang, except = c("string", ":="))
NULL

# To update namespace correctly
# attachment::att_amend_desc()

.onAttach <- function(libname, pkgname) {

  # Set the default number of cores
  set_cores()

  # Report package version
  cli_alert_info(paste0("fusionACS package v", utils::packageVersion(pkgname), " (https://ummel.github.io/fusionACS)\n"))

  # Check if a microdata directory is specified and up-to-date
  p <- try(get_directory(), silent = TRUE)
  if (!dir.exists(p)) {
    cli_alert_warning("No microdata directory specified. See ?get_microdata")
  } else {
    cli_alert_info(paste0("Microdata directory: ", p))
    dictionary(directory = p, verbose = TRUE)  # Report dictionary summary
    pbr <- piggyback::pb_releases("ummel/fusionACS", verbose = FALSE)
    if (basename(p) != pbr$release_name[1]) {
      cli_alert_warning(paste0("A more recent microdata release is available: ", pbr$release_name[1], "\n",
                               "Use get_microdata() to update to the latest release"))
    }
  }

}
