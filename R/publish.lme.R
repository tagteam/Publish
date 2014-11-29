##' @S3method publish lme
publish.lme <- function (object, adjustSigma = TRUE, verbose = FALSE, digits=2, pvalue.digits=4,eps=.0001, ...) 
{
  fixed <- lme4::fixef(object)
  stdFixed <- sqrt(diag(as.matrix(object$varFix)))
  object$corFixed <- array(t(object$varFix/stdFixed)/stdFixed, 
                           dim(object$varFix), list(names(fixed), names(fixed)))
  if (object$method == "ML" && adjustSigma == TRUE) {
    stdFixed <- sqrt(object$dims$N/(object$dims$N - length(stdFixed))) * 
      stdFixed
  }
  tTable <- data.frame(format(fixed,digits=digits,nsmall=digits), format(stdFixed,digits=digits,nsmall=digits), object$fixDF[["X"]], 
                       format(fixed/stdFixed,digits=digits,nsmall=digits), fixed)
  dimnames(tTable) <- list(names(fixed), c("Value", "Std.Error", 
                                           "DF", "t-value", "p-value"))
  tTable[, "p-value"] <- 2 * pt(-abs(tTable[, "t-value"]), 
                                tTable[, "DF"])
  tTable[, "p-value"] <- sapply(tTable[, "p-value"],format.pval,digits=pvalue.digits,eps=eps)

  ##   resd <- resid(object, type = "pearson")
  ##   if (length(resd) > 5) {
  ##     resd <- quantile(resd, na.rm = TRUE)
  ##     names(resd) <- c("Min", "Q1", "Med", "Q3", "Max")
  ##   }
  if (verbose==TRUE)
    publish(tTable,rownames=TRUE,col1name="Factor")
  ##     object$residuals <- resd
  ##     aux <- logLik(object)
  ##     object$BIC <- BIC(aux)
  ##     object$AIC <- AIC(aux)
  ##     attr(object, "oClass") <- class(object)
  ##     attr(object, "verbose") <- verbose
  ##     class(object) <- c("summary.lme", class(object))
  ##     object
  tTable
}
