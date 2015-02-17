##' Summarize confidence intervals 
##'
##' This format of the confidence intervals is user-manipulable.
##' @title Summarize confidence intervals
##' @param object Object containing point estimates and the corresponding
##' confidence intervals
##' @param digits Digits for round values
##' @param se If \code{TRUE} add standard error.
##' @param print Logical: if \code{FALSE} do not actually print
##' confidence intervals but just return them invisibly.
##' @param ... passed to print
##' @param format A string which indicates the format used for confidence intervals.
##' The string is passed to \code{\link{formatCI}} with two arguments: the lower and the upper
##' limit. For example \code{'(l;u)'} yields confidence intervals with round parenthesis in which
##' the upper and the lower limits are separated by semicolon.
##' @return Formatted confidence intervals
##' @seealso ci plot.ci format.ci
##' @examples
##' library(lava)
##' m <- lvm(Y~X)
##' m <- categorical(m,Y~X,K=4)
##' set.seed(4)
##' d <- sim(m,24)
##' ci.mean(Y~X,data=d)
##' x <- summary(ci.mean(Y~X,data=d))
##' x
#' @method summary ci
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
summary.ci <- function(object,digits=3,format,se=FALSE,print=TRUE,...){
    if (missing(format) || is.null(format)) format <- "[u;l]"
    out <- print(object,digits=3,format=format,se=FALSE,print=FALSE,...)
    out
}
