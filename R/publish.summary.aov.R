##' @method publish summary.aov
publish.summary.aov <- function(object,digits=4,pvalue.digits=4,eps=0.0001,...){
    y <- object[[1]]
    yy <- cbind(Df=y$Df,
                "F statistic"=format(y$"F value",digits=digits-2,nsmall=digits-2),
                "p-value"=format.pval(y$"Pr(>F)",digits=pvalue.digits,eps))
    rownames(yy) <- rownames(object[[1]])
    publish(yy,rownames=TRUE,colnames=TRUE,col1name="Factor")
}
