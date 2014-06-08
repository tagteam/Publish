##' Publish provides summary functions for data
##' and results of statistical analysis in ready-for-publication
##' design
##'
##' Some warnings are currently suppressed.
##' @title Publishing tables and figures
##' @param x 
##' @param ... 
##' @return Tables and figures
##' @author Thomas Gerds
##' @export
publish <- function (object, ...) {
  UseMethod("publish")
}
