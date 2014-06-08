##' Wrapper for \code{publish(...,org=TRUE)}
##'
##' 
##' @title Wrapper function for publish with output format org
##' @param x 
##' @param ... 
##' @return See publish
##' @author Thomas Alexander Gerds
##' @export
org <- function(x,...){
    publish(x,...,org=TRUE)
}
