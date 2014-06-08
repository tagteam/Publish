##' Compute mean values with confidence intervals
##'
##' Normal approximation 
##' @title Compute mean values with confidence intervals
##' @param x
##' @param ...
##' @return a list with mean values and confidence limits
##' @export
ci.mean <- function(x,...){
  UseMethod("ci.mean",object=x)
}
