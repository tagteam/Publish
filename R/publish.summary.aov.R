##' Format summary table of aov results
##'
##' Format summary table of aov results
##' @export 
##' @param object glm object
##' @param print Logical. Decide about whether or not to print the results. 
##' @param handler see \code{pubformat}
##' @param digits see \code{pubformat}
##' @param nsmall see \code{pubformat}
##' @param ... used to transport further arguments
##' @examples
##'  
##' data(Diabetes)
##' f <- glm(bp.1s~age+chol+gender+location,data=Diabetes)
##' publish(summary(aov(f)),digits=c(1,2))
##' 
publish.summary.aov <- function(object,
                                print=TRUE,
                                handler="sprintf",
                                digits=c(2,4),
                                nsmall=digits,
                                ...){
    y <- object[[1]]
    if (length(digits)==1) digits <- rep(digits,2)
    pvalue.defaults <- list(digits=digits[[2]],eps=10^{-digits[[2]]},stars=FALSE)
    smartF <- prodlim::SmartControl(call=list(...),
                                    keys=c("pvalue"),
                                    ignore=c("object","print","handler","digits","nsmall"),
                                    defaults=list("pvalue"=pvalue.defaults),
                                    forced=list("pvalue"=list(y$"Pr(>F)")),
                                    verbose=FALSE)
    yy <- cbind(Df=y$Df,
                "F statistic"= pubformat(y$"F value",handler=handler,digits=digits[[1]],nsmall=nsmall[[1]]),
                "p-value"=do.call("format.pval",smartF$pvalue))
    rownames(yy) <- rownames(object[[1]])
    ## remove residual line
    yy <- yy[-NROW(yy),,drop=FALSE]
    if (print)
        publish(yy,rownames=TRUE,colnames=TRUE,col1name="Factor",...)
    invisible(yy)
}
