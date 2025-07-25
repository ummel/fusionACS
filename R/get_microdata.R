#' Download fusionACS microdata
#'
#' By default, the latest (i.e. most recent) \href{https://github.com/ummel/fusionACS/releases}{microdata release} is downloaded to the \link[rappdirs:user_data_dir]{user data directory}. The default directory path is updated automatically and accessible via \code{\link{get_directory}}.
#'
#' @param version Version (release date) of the desired GitHub release. Defaults to the latest (i.e. most recent) microdata release. Passed to \code{\link[piggyback:pb_download]{pb_download}} internally.
#' @param overwrite Logical. Can existing version on disk be overwritten?
#' @return Message to console if successful.
#' @examples
#' \dontrun{
#' get_microdata()
#' }
#' @export

get_microdata <- function(version = "latest", overwrite = FALSE) {

  # Check inputs
  stopifnot(is.character(version), is.logical(overwrite))
  pbr <- piggyback::pb_releases("ummel/fusionACS")
  valid <- c('latest', pbr$tag_name)
  if (!version %in% valid) cli_abort(c("Invalid 'version' argument. Valid entries are:", paste(valid, collapse = ", ")))
  tag <- ifelse(version == "latest", pbr$tag_name[1], version)

  # Identify correct download location
  # Get user data directory for your package
  data.dir <- rappdirs::user_data_dir(appname = "fusionACS")
  data.dir <- normalizePath(data.dir, winslash = "/", mustWork = FALSE)

  # Create the package data directory, if necessary
  # This is typically only necessary once; otherwise it is ignored
  dir.create(data.dir, showWarnings = FALSE)

  # Check if latest version is already present
  if (!overwrite) {
    rname <- gsub(" ", "_", subset(pbr, tag_name == tag)$release_name, fixed = TRUE)  # Requested release name (as appears on disk)
    rname <- sub("_Data_", "_data_", rname)
    d <- list.dirs(data.dir, full.names = TRUE, recursive = FALSE)  # Current releases on disk
    if (rname %in% basename(d)) cli_abort("The requested version is already installed. Use 'overwrite = TRUE' to replace the data already on disk.")
  }

  # Download the .tar file for the latest release
  dest.dir <- file.path(data.dir, paste0("fusionACS_data_", tag))
  unlink(dest.dir, recursive = TRUE)
  dir.create(dest.dir, showWarnings = FALSE)
  piggyback::pb_download(file = NULL,
                         dest = dest.dir,
                         repo = "ummel/fusionACS",
                         tag = tag)

  # The directory where the files are untarred to
  # If this already exists, delete it and allow untar() to recreate below
  # dir <- sub(".tar$", "", file.path(data.dir, fname))
  # unlink(dir, recursive = TRUE)

  # Extract the .tar files
  cli_alert("Extracting the downloaded .tar files\n")
  tar.files <- list.files(dest.dir, full.names = TRUE)
  for (x in tar.files) utils::untar(tarfile = x, exdir = dest.dir)

  # Remove the original .tar files
  unlink(tar.files)

  # # Update the default data path in .Rprofile and report change
  set_directory(dest.dir)
  cli_alert_success(paste0("Data saved to: ", dest.dir, "\nThis is now the default fusionACS data directory. See ?get_directory"))

  # This simply reports the dictionary summary
  dictionary(verbose = TRUE)

}
