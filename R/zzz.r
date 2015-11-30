#' @importFrom utils available.packages contrib.url install.packages
#'   installed.packages menu modifyList packageDescription
#'   packageVersion remove.packages
NULL

#' R OANDA Package
#'
#' @section Package options:
#'
#' Roanda uses the following \code{\link{options}} to configure behaviour:
#'
#' \itemize{
#'   \item \code{Roanda.path}: path to use for \code{\link{dev_mode}}
#'
#'   \item \code{Roanda.name}: your name, used when signing draft
#'     emails.
#'
#'   \item \code{Roanda.desc.author}: a string providing a default Authors@@R
#'     string to be used in new \file{DESCRIPTION}s.  Should be a R code, and
#'     look like \code{"Hadley Wickham <h.wickham@@gmail.com> [aut, cre]"}. See
#'     \code{\link[utils]{as.person}} for more details.
#'
#'   \item \code{Roanda.desc.license}: a default license string to use for
#'     new packages.
#'
#'   \item \code{Roanda.desc.suggests}: a character vector listing packages to
#'     to add to suggests by defaults for new packages.
#
#'   \item \code{Roanda.desc}: a named list listing any other
#'     extra options to add to \file{DESCRIPTION}
#'
#' }
#' @docType package
#' @name Roanda
NULL

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.Roanda <- list(
    Roanda.path = "~/R-dev",
    Roanda.install.args = "",
    Roanda.name = "Your name goes here",
    Roanda.desc.author = 'person("First", "Last", email = "first.last@example.com", role = c("aut", "cre"))',
    Roanda.desc.license = "What license is it under?",
    Roanda.desc.suggests = NULL,
    Roanda.desc = list(),
    Roanda.revdep.libpath = file.path(tempdir(), "R-lib"),
    Roanda.oandatoken = Sys.getenv("OANDA_API_TOKEN"),
    Roanda.oandaaccountid = Sys.getenv("OANDA_API_ACCOUNT_ID"),
    Roanda.awsaccesskey = Sys.getenv("AWS_ACCESS_KEY_ID"),
    Roanda.awssecretkey = Sys.getenv("AWS_SECRET_KEY")
  )

  toset <- !(names(op.Roanda) %in% names(op))
  if (any(toset))
    options(op.Roanda[toset])


  invisible()
}

.onUnload <- function(libpath) {
  logging::logwarn("onUnload called. Unloading dynamic libraries.")
  library.dynam.unload("Roanda", libpath)
}


