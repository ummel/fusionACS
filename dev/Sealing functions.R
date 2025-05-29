# Function to 'seal' a function along with necessary global objects and package dependencies
library(globals)

seal_function <- function(fun, file, env = environment(fun)) {
  stopifnot(is.function(fun))

  # 1. Detect globals
  deps <- globalsOf(fun, envir = env)
  dep_names <- names(deps)

  # 2. Detect required packages
  required_pkgs <- packagesOf(deps)

  # 3. Seal the environment
  sealed_env <- new.env(parent = baseenv())
  for (name in dep_names) {
    assign(name, get(name, envir = env), envir = sealed_env)
  }
  environment(fun) <- sealed_env

  # 4. Save function and metadata
  metadata <- list(
    fun = fun,
    packages = unique(required_pkgs),
    timestamp = Sys.time(),
    r_version = R.version.string,
    session = sessionInfo()
  )

  saveRDS(metadata, file = file)
  invisible(file)
}

# Function to "un-seal" a sealed package
unseal_function <- function(file, install_missing = TRUE) {
  obj <- readRDS(file)

  if (!is.function(obj)) {
    fun <- obj$fun
    packages <- obj$packages
  } else {
    fun <- obj
    packages <- character(0)
  }

  # Optionally install required packages
  if (install_missing && length(packages) > 0) {
    missing_pkgs <- setdiff(packages, rownames(installed.packages()))
    if (length(missing_pkgs) > 0) {
      message("Installing missing packages: ", paste(missing_pkgs, collapse = ", "))
      install.packages(missing_pkgs)
    }
  }

  fun
}

# # Example usage
# # Original context
# library(dplyr)
#
# lookup <- tibble(np = 1:3, val = LETTERS[1:3])
# fun <- function(data) inner_join(data, lookup, by = "np")
# test0 <- fun(data)
#
# # Package it
# path.rds <- seal_function(fun, "test_sealed_function.rds")
#
# # New environment without access to original function or global 'lookup'
# rm(fun, lookup)
# fun2 <- unseal_function("test_sealed_function.rds")
#
# test2 <- fun2(data)
#
# # Are they identical?
# identical(test0, test2)
