##' Print confidence intervals
##'
##' This format of the confidence intervals is user-manipulable.
##' @title Print confidence intervals
##' @param x Object containing point estimates and the corresponding
##' confidence intervals
##' @param se If \code{TRUE} add the standard error.
##' @param print Logical: if \code{FALSE} do not actually print
##' confidence intervals but just return them invisibly.
##' @param ... passed to summary.ci
##' @return A string: the formatted confidence intervals
##' @seealso ci plot.ci formatCI summary.ci
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
