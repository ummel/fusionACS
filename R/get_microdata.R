#' Download fusionACS microdata
#'
#' By default, the latest (i.e. most recent) \href{https://github.com/ummel/fusionACS/releases}{microdata release} is downloaded to the \link[rappdirs:user_data_dir]{user data directory}. The default directory path is updated automatically and accessible via \code{\link{get_directory}}.
#'
#' @param tag Tag of the desired GitHub release. Defaults to the latest (i.e. most recent) microdata release. Passed to \code{\link[piggyback:pb_download]{pb_download}} internally.
#' @return Message to console if successful.
#' @examples
#' \dontrun{
#' get_microdata()
#' }
#' @export

get_microdata <- function(tag = "latest") {

  # Identify correct download location
  # Get user data directory for your package
  data.dir <- rappdirs::user_data_dir(appname = "fusionACS")
  data.dir <- normalizePath(data.dir, winslash = "/", mustWork = FALSE)

  # Create the package data directory, if necessary
  # This is typically only necessary once; otherwise it is ignored
  dir.create(data.dir, showWarnings = FALSE)

  # Download the .tar file for the latest release
  fname <- grep("^fusionACS_data", piggyback::pb_list(repo = "ummel/fusionACS", tag = tag)$file_name, value = TRUE)
  if (length(fname) != 1) stop("Did not identify exactly one 'fusionACS_data' file to download from repository release.")
  piggyback::pb_download(file = fname,
                         dest = data.dir,
                         repo = "ummel/fusionACS",
                         tag = tag)

  # The directory where the files are untarred to
  # If this exists, delete it and allow untar() to recreate below
  dir <- sub(".tar$", "", file.path(data.dir, fname))
  unlink(dir, recursive = TRUE)

  # Extract the .tar file
  # If the same data version already exists, this will simply overwrite
  utils::untar(tarfile = file.path(data.dir, fname), exdir = data.dir)

  # Remove the original .tar file
  unlink(file.path(data.dir, fname))

  # Report where cat was stored
  cat("Data saved to:", dir)

  # # Update the default data path in .Rprofile and report change
  set_directory(dir)
  cat("\nThis is now the default fusionACS data directory. See: get_directory()")

}
