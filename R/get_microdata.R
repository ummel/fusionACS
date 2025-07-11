#' Download fusionACS microdata
#'
#' By default, the latest (i.e. most recent) \href{https://github.com/ummel/fusionACS/releases}{microdata release} is downloaded to the \link[rappdirs:user_data_dir]{user data directory}. The default directory path is updated automatically and accessible via \code{\link{get_directory}}.
#'
#' @param version Version (release date) of the desired GitHub release. Defaults to the latest (i.e. most recent) microdata release. Passed to \code{\link[piggyback:pb_download]{pb_download}} internally.
#' @return Message to console if successful.
#' @examples
#' \dontrun{
#' get_microdata()
#' }
#' @export

get_microdata <- function(version = "latest", overwrite = FALSE) {

  # Check inputs
  stopifnot(is.character(version), is.logical(overwrite))
  pbr <- pb_releases("ummel/fusionACS")
  valid <- c('latest', pbr$tag_name)
  if (!version %in% valid) stop("Invalid 'version' argument. Valid entries are:\n", paste(valid, collapse = "\n"))
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
    if (rname %in% basename(d)) stop("The requested version is already installed at:\n", d[basename(d) == rname], "\nUse 'overwrite = TRUE' to replace the data already on disk.")
  }

  # Download the .tar file for the latest release
  fname <- grep("^fusionACS_data", piggyback::pb_list(repo = "ummel/fusionACS", tag = tag)$file_name, value = TRUE)
  if (length(fname) != 1) stop("Did not identify exactly one 'fusionACS_data' file to download from repository release.")
  piggyback::pb_download(file = fname,
                         dest = data.dir,
                         repo = "ummel/fusionACS",
                         tag = tag)

  # The directory where the files are untarred to
  # If this already exists, delete it and allow untar() to recreate below
  dir <- sub(".tar$", "", file.path(data.dir, fname))
  unlink(dir, recursive = TRUE)

  # Extract the .tar file
  # If the same data version already exists, this will simply overwrite
  cat("Extracting the downloaded .tar file\n")
  utils::untar(tarfile = file.path(data.dir, fname), exdir = data.dir)

  # Remove the original .tar file
  unlink(file.path(data.dir, fname))

  # Report where cat was stored
  cat("Data saved to:", dir)

  # # Update the default data path in .Rprofile and report change
  set_directory(dir)
  cat("\nThis is now the default fusionACS data directory. See: get_directory()")

}
