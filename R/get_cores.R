#' Get and set the number of cores
#'
#' Control the number of cores used by fusionACS for parallel processing. The default (set at package load) is one less than the total number of cores detected by \link[parallelly::availableCores]{availableCores}, which is generally accurate for both local machines and high-performance compute (HPC) clusters.
#'
#' @return  Number of cores to use.
#' @name get_cores
#' @export

get_cores<- function() {
  getOption("fusionACS.cores")
}

#' @param n Number of cores to use.
#' @rdname get_cores
#' @export

set_cores <- function(n) {

  # Default behavior: one less than the total detected
  if (missing(n)) {
    n <- parallelly::availableCores(logical = FALSE)
    if (is.na(n)) n <- parallelly::availableCores(logical = TRUE)
    n <- max(1, n - 1L)
  }

  # Check for positive integer
  if (!all(length(n) == 1, is.finite(n), n == floor(n), n > 0))
    stop("'n' must be a positive integer")

  # Set the option in the current R session
  options(fusionACS.cores = n)

  invisible(TRUE)

}
