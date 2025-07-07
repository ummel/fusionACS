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

.onLoad <- function(libname, pkgname) {

  # Set the default number of cores
  set_cores()

}
