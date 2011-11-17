publish.glm <- function(object,
                        digits=2,
                        pvalDigits=4,
                        eps=0.0001,
                        missing=TRUE,
                        sel=NULL,
                        drop,
                        intercept=0,
                        print=TRUE,
                        transform=NULL,
                        profile=TRUE,
                        ...){
  if (!is.null(object$family$family) && object$family$family=="binomial"){
    D <- object$model
    response <- D[,as.character(object$formula[[2]])]
    tmp <- as.matrix(table(response))
    tmp <- cbind(rownames(tmp),tmp)
    colnames(tmp) <- c("Response","N")
    publish(tmp,rownames=FALSE,...)
    cat("\n")
  }
  x <- data.frame(summary(object)$coefficients)
  names(x) <- c("Estimate","StandardError","tValue","pValue")
  x$Estimate=round(x$Estimate,digits)
  x$StandardError=round(x$StandardError,digits)
  if (profile==TRUE)
    ciX <- suppressMessages(confint(object))
  else{
    ctable <- coef(summary(object))
    lower <- ctable[,1]- qnorm(.975)*ctable[,2]
    upper <- ctable[,1]+ qnorm(.975)*ctable[,2]
    ciX <- cbind(lower,upper)
  }
  if (((!is.null(object$family$family)) && object$family$family=="binomial" && object$family$link=="logit")||((!is.null(transform)) &&(transform=="exp"))){
    if (is.null(sel)){
      sel <- c("OddsRatio","StandardError","CI.95","pValue","missing")
    }
    names(x)[1] <- "OddsRatio"
    x$OddsRatio=round(exp(x$OddsRatio),digits)
    ciX <- round(exp(ciX),digits)
  }
  else{
    if (is.null(sel)){
      sel <- c("Estimate","StandardError","CI.95","pValue","missing")
    }
  }
  x$CI.95=format.ci(ciX[,1],ciX[,2],digits=digits,style=2)
  x$tValue=round(x$tValue,digits)
  x$pValue=sapply(x$pValue,format.pval,digits=pvalDigits,eps=eps)
  if (missing==TRUE && length(object$na.action)>0){ 
    x$missing=0
  }
  if (match("missing",sel,nomatch=FALSE) && length(object$na.action)>0){
    x$missing=length(object$na.action)
  }
  found <- match(sel,names(x),nomatch=FALSE)!=0
  sel <- sel[found]
  if (!missing(drop)) x=x[-drop,sel]
  else
    if (intercept==0) x=x[-1,sel]
    else x=x[,sel]
  if (print==TRUE){
    out <- publish.matrix(x,col1name="Factor",...)
  }
  invisible(x)  
}
