## based on a copy from print.survdiff, tag, 07 Aug 2009 (11:19)
publish.survdiff <- function (x, digits = max(options()$digits - 4, 3), verbose=TRUE,...) {
  saveopt <- options(digits = digits)
  on.exit(options(saveopt))
  if (!inherits(x, "survdiff")) 
    stop("Object is not the result of survdiff")
  ##     if (!is.null(cl <- x$call)) {
  ##         cat("Call:\n")
  ##         dput(cl)
  ##         cat("\n")
  ##     }
  omit <- x$na.action
  if (length(omit)) 
    cat("n=", sum(x$n), ", ", naprint(omit), ".\n\n", sep = "")
  if (length(x$n) == 1) {
    z <- sign(x$exp - x$obs) * sqrt(x$chisq)
    temp <- c(x$obs, x$exp, z, signif(1 - pchisq(x$chisq, 
                                                 1), digits))
    names(temp) <- c("Observed", "Expected", "Z", "p")
    if (verbose==TRUE)
      print(temp)
  }
  else {
    if (is.matrix(x$obs)) {
      otmp <- apply(x$obs, 1, sum)
      etmp <- apply(x$exp, 1, sum)
    }
    else {
      otmp <- x$obs
      etmp <- x$exp
    }
    df <- (sum(1 * (etmp > 0))) - 1
    temp <- cbind(x$n, otmp, etmp, ((otmp - etmp)^2)/etmp, 
                  ((otmp - etmp)^2)/diag(x$var))
    dimnames(temp) <- list(names(x$n), c("N", "Observed", 
                                         "Expected", "(O-E)^2/E", "(O-E)^2/V"))
    if (verbose==TRUE){
      publish(temp,digits=digits,col1name="Log-rank test")
      cat("\n Chisq=",
          format(round(x$chisq, 1)),
          " on",
          df, 
          "degrees of freedom, p=",
          format(signif(1 - pchisq(x$chisq,df), digits)),
          "\n")
    }
  }
  attr(temp,"p-value") <- 1 - pchisq(x$chisq,df)
  invisible(temp)
}
