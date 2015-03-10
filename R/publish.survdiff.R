## based on a copy from print.survdiff, tag, 07 Aug 2009 (11:19)
#' Alternative summary of survdiff results 
#' 
#' @title Alternative summary of survdiff results 
##' @param object Object obtained with \code{survival::survdiff}.
##' @param digits Vector with digits for rounding numbers: the second for pvalues, the first for all other numbers.
##' @param print  If \code{FALSE} do not print results.
##' @param ... Not (yet) used.
##' @examples
##' library(survival)
##' data(pbc)
##' sd <- survdiff(Surv(time,status!=0)~sex,data=pbc)
##' publish(sd)
##' publish(sd,digits=c(3,2))
##' 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
##' @export
publish.survdiff <- function (object, digits = c(2,4),print=TRUE,...) {
    if (length(digits)==1) digits <- rep(digits,2)
    saveopt <- options(digits = digits)
    on.exit(options(saveopt))
    if (!inherits(object, "survdiff")) 
        stop("Object is not the result of survdiff")
    ##     if (!is.null(cl <- object$call)) {
    ##         cat("Call:\n")
    ##         dput(cl)
    ##         cat("\n")
    ##     }
    omit <- object$na.action
    if (length(omit)) 
        cat("n=", sum(object$n), ", ", naprint(omit), ".\n\n", sep = "")
    if (length(object$n) == 1) {
        z <- sign(object$exp - object$obs) * sqrt(object$chisq)
        temp <- c(object$obs, object$exp, z, format.pval(1 - pchisq(object$chisq,1),digits=digits,eps=10^{-digits[[2]]}))
        names(temp) <- c("Observed", "Expected", "Z", "p")
        if (print==TRUE)
            print(temp)
    }
    else {
        if (is.matrix(object$obs)) {
            otmp <- apply(object$obs, 1, sum)
            etmp <- apply(object$exp, 1, sum)
        }
        else {
            otmp <- object$obs
            etmp <- object$exp
        }
        df <- (sum(1 * (etmp > 0))) - 1
        temp <- cbind(object$n, otmp, etmp, ((otmp - etmp)^2)/etmp, 
                      ((otmp - etmp)^2)/diag(object$var))
        dimnames(temp) <- list(names(object$n), c("N", "Observed", 
                                             "Expected", "squared(O-E)/E", "squared(O-E)/V"))
        if (print==TRUE){
            publish(temp,digits=digits[[1]],col1name="Log-rank test")
            cat("\n Chisq=",
                format(object$chisq, digits=digits[[1]]),
                " on",
                df, 
                "degrees of freedom, p=",
                format.pval(1 - pchisq(object$chisq,df),digits=digits[[2]],eps=10^{-digits[[2]]}),
                "\n")
        }
    }
    attr(temp,"p-value") <- 1 - pchisq(object$chisq,df)
    invisible(temp)
}
