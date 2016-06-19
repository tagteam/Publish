##' Preparing a publishable table from riskRegression results
##'
##' 
##' @title Publishing results of riskRegression
##' @param object object of class riskRegression as obtained with
##' functions ARR and LRR.
##' @param digits Number of digits for regression coefficients
##' @param print If \code{FALSE} do not print the results
##' @param ... passed to \code{\link{publish.matrix}}
##' @return Table with regression coefficients, confidence intervals and p-values
##' @seealso ARR LRR
##' @examples
##' library(prodlim)
##' library(riskRegression)
##' library(lava)
##' library(survival)
##' set.seed(20)
##' d <- SimCompRisk(20)
##' f <- ARR(Hist(time,event)~X1+X2,data=d,cause=1)
##' publish(f)
##' publish(f,digits=c(1,3))
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
publish.riskRegression <- function(object,
                                   digits=c(2,4),
                                   print=TRUE,
                                   ...) {
    if (length(digits)==1) digits <- rep(digits,2)
    sv <- summary(object,verbose=FALSE,digits=digits[[1]],eps=10^{-digits[[2]]})
    out <- sv[,c("Factor","exp(Coef)","CI_95","Pvalue")]
    modeltype <- if (as.name("LRR")==object$call[[1]]) "LRR" else "ARR"
    colnames(out) <- c("Factor",modeltype,"CI_95","p-value")
    if (print) publish(out,...)
    invisible(out)
}
