publish.summary.aov <- function(x,pvalDigits=4,eps=0.0001){
  y <- x[[1]]
  yy <- cbind(Df=y$Df,"F statistic"=format(y$"F value",digits=digits-2,nsmall=digits-2),"p-value"=format.pval(y$"Pr(>F)",digits=pvalDigits,eps))
  rownames(yy) <- rownames(x[[1]])
  publish(yy,rownames=TRUE,colnames=TRUE,col1name="Factor")
}
