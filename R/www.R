#' This function adds a resource path for the shiny application.
#'
#' @param libname The name of the package's library where the resource path will be added.
#' @param pkgname The name of the package where the resource path will be added.
#'
#' @return NULL
#'
#' @details This function adds a resource path for the shiny application using the \code{addResourcePath} function from the shiny package. It adds a directory path to the 'www' folder of the package using the \code{system.file} function.
#'
#' @examples
#' .onLoad(libname = "mylib", pkgname = "mypkg")


.onLoad <- function(libname, pkgname) {
  shiny::addResourcePath(
    prefix = "www",
    directoryPath = system.file(
      "www",
      package = "interpretablePA"
    )
  )
}


#' Unload Function for interpretablePA Package
#'
#' This function is called when the interpretablePA package is unloaded. It removes the resource path "www" previously added with \code{\link{.onLoad}} using the \code{\link[shiny]{removeResourcePath}} function from the shiny package.
#'
#' @param libname character string specifying the path to the library being unloaded.
#' @param pkgname character string specifying the name of the package being unloaded.
#' @return NULL
#' @export

.onUnload <- function(libname, pkgname) {
  shiny::removeResourcePath("www")
}
