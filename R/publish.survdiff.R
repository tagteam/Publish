## based on a copy from print.survdiff, tag, 07 Aug 2009 (11:19)
##' @export
publish.survdiff <- function (object, digits = 4, eps=0.0001,verbose=TRUE,...) {
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
        temp <- c(object$obs, object$exp, z, format.pval(1 - pchisq(object$chisq,1),digits=digits,eps=eps))
        names(temp) <- c("Observed", "Expected", "Z", "p")
        if (verbose==TRUE)
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
        if (verbose==TRUE){
            publish(temp,digits=1,col1name="Log-rank test")
            cat("\n Chisq=",
                format(object$chisq, digits=1),
                " on",
                df, 
                "degrees of freedom, p=",
                format.pval(1 - pchisq(object$chisq,df),digits=digits,eps=eps),
                "\n")
        }
    }
    attr(temp,"p-value") <- 1 - pchisq(object$chisq,df)
    invisible(temp)
}
