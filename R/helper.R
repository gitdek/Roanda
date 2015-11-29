###############################################################################
# Roanda: helper source
#   By puglisij Copyright (C) 2015, All rights reserved.
#
###############################################################################


os.specific <- new.env()

.onAttach <- function(libname, pkgname) {
  os.specific$os <- R.Version()$os
  if (R.Version()$os == "mingw32") {
    os.specific$suffix <- ".cmd"
  } else {
    os.specific$suffix <- ""
  }

  initLogging()

  #global.assign('Global.logInit', TRUE, environ = .GlobalEnv)


}

.onUnload <- function(libpath) {
  logging::logwarn("onUnload called. Unloading dynamic libraries.")
  #library.dynam.unload("Roanda", libpath)
}


#' Shortcut for global variable assignment with logging and optional ZMQ storage backend.
#'
#' @param x A variable name.
#' @param value value assigned to x.
#' @param environ the environment to use.
#'
#' @export
global.assign <- function(x, value, environ = .GlobalEnv) {
  assign(x,value,envir = environ)
  logging::logdebug(paste("Global assignment:",x,sep = ' '))

}


#' Attempt to load the package, and if unsuccessful try to install.
#'
#' @family helper functions
#' @param pkg Package name
#' @param quiet if TRUE suppresses output from this function.
#' @examples
#' package.require('quantmod')
#' @export
package.require <- function(pkg = ".", quiet = TRUE)
{
  if (!require(pkg,character.only = TRUE,quietly = quiet))
  {
    install.packages(pkg,dep = TRUE,quiet = quiet,verbose = quiet)
    if (!require(pkg,character.only = TRUE,quietly = quiet))
      stop("Package not found")
  }
}
