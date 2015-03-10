##' Print confidence intervals
##'
##' This format of the confidence intervals is user-manipulable.
##' @title Print confidence intervals
##' @param x Object containing point estimates and the corresponding
##' confidence intervals
##' @param digits Digits for round values
##' @param format A string which indicates the format used for confidence intervals.
##' The string is passed to \code{\link{formatCI}} with two arguments: the lower and the upper
##' limit. For example \code{'(l;u)'} yields confidence intervals with round parenthesis in which
##' the upper and the lower limits are separated by semicolon.
##' @param se If \code{TRUE} add the standard error.
##' @param print Logical: if \code{FALSE} do not actually print
##' confidence intervals but just return them invisibly.
##' @param ... passed to print
##' @return A string: the formatted confidence intervals
##' @seealso ci plot.ci formatCI
##' @examples
##' library(lava)
##' m <- lvm(Y~X)
##' m <- categorical(m,Y~X,K=4)
##' set.seed(4)
##' d <- sim(m,24)
##' ci.mean(Y~X,data=d)
##' x <- ci.mean(Y~X,data=d)
##' print(x,format="(l,u)")
##' @export
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
print.ci <- function(x,se=FALSE,print=TRUE,...){
  summary(x,se=se,print=print,...)
}
