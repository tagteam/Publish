##' Publish cause-specific Cox models
##'
##' The cause-specific hazard ratio's are combined into one table.
##' @title Tabulizing cause-specific hazard ratio from all causes with confidence limits and Wald test p-values.
##' @param object Cause-specific hazard model obtained with
##' \code{CSC}.
##' @param cause Show a table for this cause. If omitted, list all
##' causes.
##' @param confint.method See \code{regressionTable}
##' @param pvalue.method See \code{regressionTable}
##' @param factor.reference See \code{regressionTable}
##' @param units See \code{regressionTable}
##' @param print If \code{TRUE} print the table(s).
##' @param ... passed on to control formatting of parameters,
##' confidence intervals and p-values.  See
##' \code{summary.regressionTable}.
##' @return Table with cause-specific hazard ratios, confidence limits and p-values.
##' @author Thomas Alexander Gerds <tab@@biostat.ku.dk>
##' @examples
##' library(riskRegression)
##' library(prodlim)
##' library(pec)
##' library(survival)
##' data(Melanoma,package="riskRegression")
##' fit1 <- CSC(list(Hist(time,status)~sex,Hist(time,status)~invasion+epicel+age),
##'             data=Melanoma)
##' publish(fit1)
##' publish(fit1,pvalue.stars=TRUE)
##' publish(fit1,factor.reference="inline",units=list("age"="years"))
##' @export
publish.CauseSpecificCox <- function(object,
                                     cause,
                                     confint.method,
                                     pvalue.method,
                                     factor.reference="extraline",
                                     units=NULL,
                                     print=TRUE,
                                     ...){
    
    if (missing(confint.method)) confint.method="default"
    if (missing(pvalue.method))
        pvalue.method=switch(confint.method,
                             "robust"={"robust"},
                             "simultaneous"={"simultaneous"},
                             "default")
    if (missing(cause)) {
        clist <- lapply(object$models,function(m){
            ## m$call$data <- object$call$data
            pm <- regressionTable(m,
                                  pvalue.method=pvalue.method,
                                  confint.method=confint.method,
                                  print=FALSE,
                                  factor.reference=factor.reference,
                                  units=units,...)
            summary.regressionTable(pm,print=FALSE,...)
        })
        cause1 <- clist[[1]]
        ## colnames(cause1) <- paste(names(object$models)[[1]],names(cause1),sep=".")
        cause2 <- clist[[2]]
        ## colnames(cause2) <- paste(names(object$models)[[2]],names(cause2),sep=".")
        out <- clist
    } else{
        m <- object$models[[cause]]
        ## m$call$data <- object$call$data
        pm <- regressionTable(m,
                              pvalue.method=pvalue.method,
                              confint.method=confint.method,
                              print=FALSE,
                              factor.reference=factor.reference,
                              units=units,...)
        ## now pm is  a regression table
        out <- summary.regressionTable(pm,print=FALSE,...)$regressionTable
    }
    if (print==TRUE) {
        print.listof(lapply(out,function(x)x$regressionTable))
    }
    invisible(out)
}

#----------------------------------------------------------------------
### publish.CauseSpecificCox.R ends here
