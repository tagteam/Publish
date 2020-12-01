### publish.MIresult.R ---
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Aug 17 2017 (09:52)
## Version:
## Last-Updated: Dec  1 2020 (16:48) 
##           By: Thomas Alexander Gerds
##     Update #: 52
#----------------------------------------------------------------------
### Code:
#' Regression tables after multiple imputations
#'
#' Show results of smcfcs based multiple imputations of missing covariates in publishable format
#' @title Present logistic regression and Cox regression  obtained with mitools::MIcombine based on smcfcs::smcfcs multiple imputation analysis 
#' @param object Object obtained with mitools::MIcombine based on smcfcs::smcfcs multiple imputation analysis 
#' @param confint.method No options here. Only Wald type confidence
#'     intervals.
#' @param pvalue.method No options here. Only Wald type tests.
#' @param digits Rounding digits for all numbers but the p-values.
#' @param print If \code{FALSE} suppress printing of the results
#' @param factor.reference Style for showing results for
#'     categorical. See \code{regressionTable}.
#' @param intercept See \code{regressionTable}.
#' @param units See \code{regressionTable}.
#' @param fit One fitted model using the same formula as
#'     \code{object}. This can be the fit to the complete case data or
#'     the fit to one of the completed data. It is used to get
#'     xlevels, formula and terms. For usage see examples.  is used to
#'     fit
#' @param data Original data set which includes the missing values
#' @param ... passed to summary.regressionTable, labelUnits and publish.default.
#' @examples
#'
#' \dontrun{
#' if (requireNamespace("riskRegression",quietly=TRUE)
#'   & requireNamespace("mitools",quietly=TRUE)
#'   & requireNamespace("smcfcs",quietly=TRUE)){
#' library(riskRegression)
#' library(mitools)
#' library(smcfcs)
#' ## continuous outcome: linear regression
#' # lava some data with missing values
#' set.seed(7)
#' d=sampleData(78)
#' ## generate missing values
#' d[X1==1,X6:=NA] 
#' d[X2==1,X3:=NA]
#' d=d[,.(X8,X4,X3,X6,X7)]
#' sapply(d,function(x)sum(is.na(x)))
#'
#' # multiple imputation (should set m to a large value)
#' 
#' set.seed(17)
#' f= smcfcs(d,smtype="lm",
#'            smformula=X8~X4+X3+X6+X7,
#'            method=c("","","logreg","norm",""),m=3)
#' ccfit=lm(X8~X4+X3+X6+X7,data=d)
#' mifit=MIcombine(with(imputationList(f$impDatasets),
#'                 lm(X8~X4+X3+X6+X7)))
#' publish(mifit,fit=ccfit,data=d)
#' publish(ccfit)
#' 
#' ## binary outcome
#' # lava some data with missing values
#' set.seed(7)
#' db=sampleData(78,outcome="binary")
#' ## generate missing values
#' db[X1==1,X6:=NA] 
#' db[X2==1,X3:=NA]
#' db=db[,.(Y,X4,X3,X6,X7)]
#' sapply(db,function(x)sum(is.na(x)))
#'
#' # multiple imputation (should set m to a large value)
#' set.seed(17)
#' fb= smcfcs(db,smtype="logistic",
#'            smformula=Y~X4+X3+X6+X7,
#'            method=c("","","logreg","norm",""),m=2)
#' ccfit=glm(Y~X4+X3+X6+X7,family="binomial",data=db)
#' mifit=MIcombine(with(imputationList(fb$impDatasets),
#'                 glm(Y~X4+X3+X6+X7,family="binomial")))
#' publish(mifit,fit=ccfit)
#' publish(ccfit)
#'
#' ## survival: Cox regression
#' library(survival)
#' # lava some data with missing values
#' set.seed(7)
#' ds=sampleData(78,outcome="survival")
#' ## generate missing values
#' ds[X5==1,X6:=NA] 
#' ds[X2==1,X3:=NA]
#' ds=ds[,.(time,event,X4,X3,X6,X7)]
#' sapply(ds,function(x)sum(is.na(x)))
#'
#' set.seed(17)
#' fs= smcfcs(ds,smtype="coxph",
#'            smformula="Surv(time,event)~X4+X3+X6+X7",
#'            method=c("","","","logreg","norm",""),m=2)
#' ccfit=coxph(Surv(time,event)~X4+X3+X6+X7,data=ds)
#' mifit=MIcombine(with(imputationList(fs$impDatasets),
#'                 coxph(Surv(time,event)~X4+X3+X6+X7)))
#' publish(mifit,fit=ccfit,data=ds)
#' publish(ccfit)
#'
#' ## competing risks: Cause-specific Cox regression 
#' library(survival)
#' # lava some data with missing values
#' set.seed(7)
#' dcr=sampleData(78,outcome="competing.risks")
#' ## generate missing values
#' dcr[X5==1,X6:=NA] 
#' dcr[X2==1,X3:=NA]
#' dcr=dcr[,.(time,event,X4,X3,X6,X7)]
#' sapply(dcr,function(x)sum(is.na(x)))
#'
#' set.seed(17)
#' fcr= smcfcs(dcr,smtype="compet",
#'            smformula=c("Surv(time,event==1)~X4+X3+X6+X7",
#'                        "Surv(time,event==2)~X4+X3+X6+X7"),
#'            method=c("","","","logreg","norm",""),m=2)
#' ## cause 2 
#' ccfit2=coxph(Surv(time,event==2)~X4+X3+X6+X7,data=dcr)
#' mifit2=MIcombine(with(imputationList(fcr$impDatasets),
#'                 coxph(Surv(time,event==2)~X4+X3+X6+X7)))
#' publish(mifit2,fit=ccfit2,data=dcr)
#' publish(ccfit2)
#' }
#'} 
#' 
#' @author Thomas A. Gerds <tag@@biostat.ku.dk>
#' @export
publish.MIresult <- function(object,
                             confint.method,
                             pvalue.method,
                             digits=c(2,4),
                             print=TRUE,
                             factor.reference="extraline",
                             intercept,
                             units=NULL,
                             fit,
                             data,
                             ...){
    pvalMIresult <- function(object){
        se <- sqrt(diag(stats::vcov(object)))
        p <- 2*stats::pnorm(-abs(object$coef/se))
        p
    }
    if (missing(fit)) stop("Need the model fitted in the complete cases.")
    object$xlevels <- fit$xlevels
    object$formula <- fit$formula
    if (missing(data)){
        if (is.null(fit$data)) stop("Need original data set via argument 'data' because argument 'fit' does not provide them.")
        else{
            object$data <- fit$data
        }
    }else object$data <- data
    object$terms <- fit$terms
    ## make sure that a coxph object is treated as such 
    class(object) <- c(class(object),class(fit))
    ## make sure that a logistic regression is treated as such
    if ('glm' %in% class(fit))
        object$family <- fit$family
    if (!missing(confint.method) && confint.method!="default") stop("Can only do simple Wald confidence intervals based on MIresults.")
    if (!missing(pvalue.method)) stop("Can only do simple Wald test p-values based on MIresults.")
    if (missing(intercept)){
        intercept <- 1*(class(fit)[1] == "lm" ||
                        (class(fit)[1]=="glm" && stats::family(fit)!="binomial"))
    }
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
