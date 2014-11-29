##' Wrapper for \code{publish(...,org=TRUE)}
##'
##' 
##' @title Wrapper function for publish with output format org
##' @param x object to format as org 
##' @param ... passed to publish
##' @return See publish
##' @author Thomas Alexander Gerds
##' @export
org <- function(x,...){
    publish(x,...,org=TRUE)
}
