### publish.MIresult.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Aug 17 2017 (09:52) 
## Version: 
## Last-Updated: Aug 17 2017 (11:28) 
##           By: Thomas Alexander Gerds
##     Update #: 14
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
#' @param object A \code{MIresults} object as obtained with \code{mitools::MIcombine}.
#' @param confint.method No options here. Only Wald type confidence intervals. 
#' @param pvalue.method No options here. Only Wald type tests.
#' @param digits Rounding digits for all numbers but the p-values.
#' @param print If \code{FALSE} suppress printing of the results
#' @param factor.reference Style for showing results for categorical. See \code{regressionTable}.
#' @param intercept See \code{regressionTable}.
#' @param units See \code{regressionTable}.
#' @param fit One fitted model using the same formula as \code{object}. This can be the fit to the complete case data or the fit to one of the completed data. It is used to get xlevels, formula and terms. For usage see examples. 
#' is used to fit 
#' @param ...
#' @export
publish.MIresult <- function(object,
                             confint.method,
                             pvalue.method,
                             digits=c(2,4),
                             print=TRUE,
                             factor.reference="extraline",
                             intercept=0,
                             units=NULL,
                             fit,
                             ...){
    pvalMIresult <- function(object){
        se <- sqrt(diag(vcov(object)))
        2*pnorm(-abs(object$coef/se))
    }
    if (missing(fit)) stop("Need the model fitted in the complete cases.")
    object$xlevels <- fit$xlevels
    object$formula <- fit$formula
    object$terms <- fit$terms
    ## the following makes sure that a coxph object is treated as such 
    class(object) <- c(class(object),class(fit))
    if (!missing(confint.method) && confint.method!="default") stop("Can only do simple Wald confidence intervals based on MIresults.")
    if (!missing(pvalue.method)) stop("Can only do simple Wald test p-values based on MIresults.")
    rt <- regressionTable(object,
                          confint.method="default",
                          pvalue.method=pvalMIresult,                          
                          factor.reference=factor.reference,
                          intercept=intercept,
                          units=units)
    srt <- summary.regressionTable(rt,
                                   digits=digits,
                                   print=FALSE,...)
    XXsrt <- do.call(labelUnits,c(list(x=srt),list(...),srt$Variable))
    if (print==TRUE)
        publish(srt$regressionTable,...)
    invisible(srt)
}


######################################################################
### publish.MIresult.R ends here
