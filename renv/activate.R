# === renv Startup ===

# This script is executed on R startup when R is launched from a directory
# containing an renv project. It is responsible for bootstrapping renv,

# and then ensuring that the project's private library is active.

# skip if we're not running interactively
# (unless RENV_CONFIG_AUTOLOADER_ENABLED is explicitly true)
if (!interactive() && !identical(Sys.getenv("RENV_CONFIG_AUTOLOADER_ENABLED"), "true"))
  return()

# skip if renv has already been loaded
if ("renv" %in% loadedNamespaces())
  return()

# constants
bootstrap.version <- "1.0.7"

# helper for reading an R environment variable
getenv <- function(name, unset = "") {
  val <- Sys.getenv(name, unset = unset)
  val <- sub("[\"]", "", val)
  sub("[\"]$", "", val)
}

# paths
root      <- getwd()
libpath   <- file.path(root, "renv/library")
bootstrap <- file.path(root, "renv/bootstrap")
lockfile  <- file.path(root, "renv.lock")

# construct library paths
`_R_LIBS_USER_` <- Sys.getenv("_R_LIBS_USER_", unset = "~/.R/library")
libpaths <- .libPaths()
nlibs <- length(libpaths)

# avoid re-initializing if path already set
if (nlibs > 0 && normalizePath(libpaths[[1L]], mustWork = FALSE) == normalizePath(libpath, mustWork = FALSE))
  return()

# create the library path if it doesn't exist
# (renv will do this later but it's cleaner to do it here)
if (!file.exists(libpath))
  dir.create(libpath, recursive = TRUE)

# ensure the library is first on the library path
.libPaths(c(libpath, libpaths))

# check for RENV_AUTOLOAD_PROJECT -- if set, then we should attempt to
# automatically load the project without confirmation
autoload <- Sys.getenv("RENV_AUTOLOAD_PROJECT")
if (nzchar(autoload) && !is.na(as.logical(autoload))) {
  if (file.exists(lockfile)) {
    tryCatch(
      renv::load(project = root),
      error = function(e) {
        warning(e)
        message("failed to load project '", root, "'")
      }
    )
  }
  return()
}

# if the user has requested that we bootstrap renv, do that now
if (identical(getenv("RENV_BOOTSTRAP_INSTALL"), "true")) {

  # attempt to install renv
  install.packages("renv",
                   lib = libpath,
                   repos = "https://cloud.r-project.org",
                   type = "source",
                   quiet = TRUE)

  # load it
  requireNamespace("renv", lib.loc = libpath, quietly = TRUE)

  # if we succeeded, then attempt to load the project
  if ("renv" %in% loadedNamespaces()) {
    Sys.unsetenv("RENV_BOOTSTRAP_INSTALL")
    return(renv::load(project = root))
  }

  #failed to bootstrap renv
  warning("failed to bootstrap renv")

}

# if we're running RStudio, then prompt the user to activate the project
# otherwise, just load it
if (identical(getenv("RSTUDIO"), "1")) {

  # bail if we're not able to load the rstudioapi
  if (!requireNamespace("rstudioapi", quietly = TRUE))
    return()

  # bail if rstudioapi isn't available
  if (!rstudioapi::isAvailable())
    return()

  # if the project hasn't been loaded, then prompt the user
  #
  # note that RStudio will normally do this automatically on
  # project load; this is just a fallback just in case
  #
  # these APIs are generally available in RStudio v1.2+
  if (rstudioapi::hasFun("getActiveProject")) {
    activeProject <- rstudioapi::getActiveProject()
    if (is.null(activeProject) || !identical(normalizePath(activeProject), normalizePath(root))) {
      if (rstudioapi::hasFun("executeCommand"))
        rstudioapi::executeCommand("activateProject", root)
    }
  }

} else if (file.exists(lockfile)) {

  # attempt to load renv
  if (requireNamespace("renv", lib.loc = libpath, quietly = TRUE))
    return(renv::load(project = root))

  # failed to load renv; attempt to bootstrap it
  bootstrap.path <- Sys.getenv("RENV_BOOTSTRAP_PATH", unset = file.path(root, "renv/bootstrap.R"))
  if (file.exists(bootstrap.path)) {
    source(bootstrap.path)
    if (requireNamespace("renv", lib.loc = libpath, quietly = TRUE))
      return(renv::load(project = root))
  }

  # failed to load or bootstrap project
  message("Failed to find an renv installation: ",
          "renv will not be active for this session.")
  message("Use `renv::activate()` to re-initialize the project.")

}
