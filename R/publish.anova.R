##' @S3method publish anova
publish.anova <- function(object,digits=2,pvalDigits=4,eps=0.0001,print=TRUE,residuals=FALSE,...){
  x <- object
  if (residuals==FALSE)
    x=x[-NROW(x),]
  names(x) <- c("Degrees of freedom","Sum of squares","Mean of squares","F value","p value")
  x$"Sum of squares"=format(x$"Sum of squares",digits=digits,nsmall=digits)
  x$"Mean of squares"=format(x$"Mean of squares",digits=digits,nsmall=digits)
  x$"F value"=format(x$"F value",digits=digits,nsmall=digits)
  x$"p value"=sapply(x$"p value",format.pval,digits=pvalDigits,eps=eps)
  if (print==TRUE){
    out <- suppressMessages(publish.matrix(x,col1name="Factor",...))
  }
  invisible(x)  
}
