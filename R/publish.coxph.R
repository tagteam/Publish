publish.coxph <- function(object,
                          conf.int = 0.95,
                          scale = 1,
                          digits = 2,
                          pvalDigits=4,
                          eps=.0001,
                          StandardError=FALSE,
                          print=TRUE,
                          ...) {
  beta <- object$coef
  ##   print(names(beta))
  ##   varnames <- attr(mRD$terms,"term.labels")
  ##   lapply(varnames,function(v){
  ##   }
  nabeta <- !(is.na(beta))
  beta2 <- beta[nabeta]
  if (is.null(beta) | is.null(object$var)) stop("Input is not valid")
  se <- sqrt(diag(object$var))
  if (!is.null(object$naive.var)) nse <- sqrt(diag(object$naive.var))
  z <- qnorm((1 + conf.int)/2, 0, 1)
  beta <- beta * scale
  se <- se * scale
  if (StandardError==TRUE)
    out <- data.frame("Hazard ratio"=format(round(exp(beta),digits)),
                      "Standard error"=format(round(se,digits)),
                      "CI.95"=paste("[",format(round(exp(beta - z * se),2)),";",format(round(exp(beta + z * se),2)),"]",sep=""),
                      "P-value"=sapply(1 - pchisq((beta/se)^2, 1),format.pval,digits=pvalDigits,eps=eps))
  else
    out <- data.frame("Hazard ratio"=format(round(exp(beta),digits)),
                      "CI.95"=paste("[",format(round(exp(beta - z * se),2)),";",format(round(exp(beta + z * se),2)),"]",sep=""),
                      "P-value"=sapply(1 - pchisq((beta/se)^2, 1),format.pval,digits=pvalDigits,eps=eps))
  if (print==TRUE){
    publish(out,...)
    if (length(z <- object$na.action)){
      cat("\nObservations deleted due to missing values:\n")
      if (attr(z,"class")=="delete"){
        zz <- cbind(names(z$nmiss[z$nmiss!=0]),matrix(z$nmiss[z$nmiss!=0],ncol=1))
        colnames(zz) <- c("Factor","Missing")
        publish(zz,col1name="Factor",rownames=FALSE)
      }
      else{
        publish(naprint(z))
      }
    }
  }
  invisible(out)
}

