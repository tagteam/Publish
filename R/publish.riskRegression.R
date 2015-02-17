##' Preparing a publishable table from riskRegression results
##'
##' 
##' @title Publishing results of riskRegression
##' @param object object of class riskRegression as obtained with functions ARR and LRR.
##' @param digits Number of digits for regression coefficients
##' @param pvalue.digits Number of digits for p-values
##' @param eps Eps for p-values
##' @param pvalue.stars If \code{TRUE} p-values are replaced by stars
##' @param showMissing If \code{TRUE} the numbers of missing values are shown in a separate column 
##' @param print If \code{FALSE} do not print the results
##' @param ... passed to \code{\link{publish.matrix}}
##' @return Table with regression coefficients, confidence intervals and p-values
##' @seealso ARR LRR
##' @examples
##' library(prodlim)
##' library(riskRegression)
##' library(lava)
##' set.seed(20)
##' d <- SimCompRisk(20)
##' \dontrun{
##' f <- ARR(Hist(time,event)~X1+X2,data=d,cause=1)
##' publish(f)
##' }
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
publish.riskRegression <- function(object,
                                   digits=2,
                                   pvalue.digits=4,
                                   eps=.0001,
                                   pvalue.stars=FALSE,
                                   showMissing="ifany",
                                   print=TRUE,
                                   ## ci.format=paste("[",paste("%1.",digits,"f",sep=""),";",paste("%1.",digits,"f",sep=""),"]",sep=""),
                                   ...) {
    sv <- summary(object,verbose=FALSE,digits=digits,eps=eps,pvalue.digits=pvalue.digits)
    out <- sv[,c("Factor","exp(Coef)","CI_95","Pvalue")]
    colnames(out) <- c("Factor","ARR","CI_95","p-value")
    if (print) publish(out,...)
    invisible(out)
}
