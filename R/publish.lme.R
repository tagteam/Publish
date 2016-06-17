publish.lme <- function (object, adjustSigma = TRUE, print = FALSE, handler="sprintf",digits=c(2,4),nsmall=digits, ...){
    fixed <- lme4::fixef(object)
    stdFixed <- sqrt(diag(as.matrix(object$varFix)))
    object$corFixed <- array(t(object$varFix/stdFixed)/stdFixed, 
                             dim(object$varFix), list(names(fixed), names(fixed)))
    if (object$method == "ML" && adjustSigma == TRUE) {
        stdFixed <- sqrt(object$dims$N/(object$dims$N - length(stdFixed))) * 
            stdFixed
    }
    tTable <- data.frame(pubformat(fixed,handler=handler,digits=digits,nsmall=digits),
                         pubformat(stdFixed,handler=handler,digits=digits,nsmall=digits),
                         object$fixDF[["X"]], 
                         pubformat(fixed/stdFixed,handler=handler,digits=digits,nsmall=digits),
                         numeric(length(fixed)))
    dimnames(tTable) <- list(names(fixed), c("Value","Std.Error","DF","t-value","p-value"))
    tTable[, "p-value"] <- 2 * pt(-abs(tTable[, "t-value"]),tTable[, "DF"])
    if (length(digits)==1) digits <- rep(digits,2)
    pvalue.defaults <- list(digits=digits[[2]],eps=10^{-digits[[2]]},stars=FALSE)
    smartF <- prodlim::SmartControl(call=list(...),
                                    keys=c("pvalue"),
                                    ignore=c("object","adjustSigma","print","handler","digits","nsmall"),
                                    defaults=list("pvalue"=pvalue.defaults),
                                    forced=list("pvalue"=list(x=tTable[, "p-value"])),
                                    verbose=FALSE)
    tTable[, "p-value"] <- do.call(format.pval,smartF$pvalue)
    ##   resd <- resid(object, type = "pearson")
    ##   if (length(resd) > 5) {
    ##     resd <- quantile(resd, na.rm = TRUE)
    ##     names(resd) <- c("Min", "Q1", "Med", "Q3", "Max")
    ##   }
    if (print==TRUE)
        publish(tTable,rownames=TRUE,col1name="Factor")
    ##     object$residuals <- resd
    ##     aux <- logLik(object)
    ##     object$BIC <- BIC(aux)
    ##     object$AIC <- AIC(aux)
    ##     attr(object, "oClass") <- class(object)
    ##     class(object) <- c("summary.lme", class(object))
    ##     object
    tTable
}
