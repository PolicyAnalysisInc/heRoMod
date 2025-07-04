# This file allows packrat (and renv) to bootstrap itself when R is started.
# It should be committed to your version control system.

# TODO: remove this check for future R versions
# nocov start
if (getRversion() < "3.5.0" && !identical(Sys.getenv("RENV_PROFILE_SKIP"), "true")) {
  dotRprofile <- file.path(Sys.getenv("HOME", unset = ".Rprofile"), ".Rprofile")
  if (file.exists(dotRprofile)) {
    tryCatch(
      source(dotRprofile),
      error = function(e) {
        warning(e)
        warning("Error sourcing user .Rprofile ",
                "(this warning will be removed once RStudio v1.2 is released)")
      }
    )
  }
}
# nocov end

source("renv/activate.R")
