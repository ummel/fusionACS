library(piggyback)

# Path to the directory to be uploaded
dir <- "fusionACS_data"

# Check and normalize the path
dir <- normalizePath(dir, winslash = "/")

# Retain original directory name
od <- dir
pd <- dirname(dir)
on <- file.path(pd, paste0("fusionACS_data_", Sys.Date()))
file.rename(od, on)

# tar() the target directory to a tempfile
wd <- getwd()
setwd(pd)
tf <- file.path(tempdir(), paste0(basename(on), ".tar"))
utils::tar(tarfile = tf, files = basename(on), compression = "none")

# Reset working directory and directory name
setwd(wd)
file.rename(on, od)

# Create a fusionACS data release for the upload
# If maintaining old versions of the data is not useful, you can stick with a single release and upload all of your data there (https://docs.ropensci.org/piggyback/articles/piggyback.html)
pb_release_create(repo = "ummel/fusionACS",
                  tag = Sys.Date(),
                  name = paste0("fusionACS Data ", Sys.Date()),
                  body = paste0("Public release of fusionACS sample data on ", Sys.Date(), ".")) # Could automate 'body' to include description of included surveys

# Upload data to most recent release
pb_upload(tf, repo = "ummel/fusionACS")

# Delete the temporary .tar file
unlink(tf)
