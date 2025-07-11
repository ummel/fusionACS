#' Get and set the default fusionACS microdata directory
#'
#' The user does not generally need to use these functions, since \code{\link{get_microdata}} sets the default directory automatically.
#'
#' @return File path.
#' @name get_directory
#' @export

get_directory <- function() {
  x <- getOption("fusionACS.data_directory")
  if (is.null(x)) stop("No fusionACS microdata directory specified")
  return(x)
}

#' @param dir Path to fusionACS data directory.
#' @rdname get_directory
#' @export

set_directory <- function(dir) {

  if (!is.null(dir)) {

    # Check if 'dir' exists
    dir <- try(normalizePath(dir, winslash = "/", mustWork = TRUE), silent = TRUE)
    if (inherits(dir, "try-error")) stop("Could not resolve 'dir' to a valid path")

    rprofile_path <- path.expand("~/.Rprofile")
    option_line <- sprintf('options(fusionACS.data_directory = "%s")', dir)

    # Read existing .Rprofile if it exists
    lines <- if (file.exists(rprofile_path)) readLines(rprofile_path) else character()

    # Remove any existing 'fusionACS.data_directory' options
    lines <- lines[!grepl("^options\\(.*fusionACS\\.data_directory.*\\)", lines)]

    # Add the new option setting to .Rprofile
    lines <- c(lines, option_line)
    lines <- lines[lines != ""]
    writeLines(lines, rprofile_path)

  }

  # Set the option in the current R session
  options(fusionACS.data_directory = dir)

  #message("Option 'fusionACS.data_directory' set persistently in ~/.Rprofile.")
  invisible(TRUE)
}
