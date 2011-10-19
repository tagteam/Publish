publish.anova <- function(object,digits=2,pvalDigits=4,eps=0.0001,print=TRUE,residuals=FALSE,...){
  x <- object
  if (residuals==FALSE)
    x=x[-NROW(x),]
  names(x) <- c("Degrees of freedom","Sum of squares","Mean of squares","F value","p value")
  x$"Sum of squares"=round(x$"Sum of squares",digits)
  x$"Mean of squares"=round(x$"Mean of squares",digits)
  x$"F value"=round(x$"F value",digits)
  x$"p value"=sapply(x$"p value",format.pval,digits=pvalDigits,eps=eps)
  if (print==TRUE){
    out <- suppressMessages(publish.matrix(x,col1name="Factor",...))
  }
  invisible(x)  
}
