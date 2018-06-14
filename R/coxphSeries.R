##' Run a series of Cox regression analyses for a list of predictor variables
##' and summarize the results in a table.
##' The Cox models can be adjusted for a fixed set of covariates
##'  
##' This function runs on \code{coxph} from the survival package.
##' @title Run a series of Cox regression models 
##' @param formula The fixed part of the regression formula. For
##' univariate analyses this is simply \code{Surv(time,status)~1}
##' where \code{Surv(time,status)} is the outcome variable. When the
##' aim is to control the effect of \code{vars} in each element of the
##' series by a fixed set of variables it is
##' \code{Surv(time,status)~x1+x2} where again Surv(time,status) is
##' the outcome and x1 and x2 are confounders.
##' @param data A \code{data.frame} in which the \code{formula} gets
##' evaluated.
##' @param vars A list of variable names, the changing part of the
##' regression formula.
##' @param ... passed to publish.coxph
##' @return matrix with results
##' @author Thomas Alexander Gerds
##' @examples
##' library(survival)
##' data(pbc)
##' ## collect hazard ratios from three univariate Cox regression analyses
##' pbc$edema <- factor(pbc$edema,levels=c("0","0.5","1"),labels=c("0","0.5","1"))
##' uni.hr <- coxphSeries(Surv(time,status==2)~1,vars=c("edema","bili","protime"),data=pbc)
##' uni.hr
##'
##' ## control the logistic regression analyses for age and gender
##' ## but collect only information on the variables in `vars'.
##' controlled.hr <- coxphSeries(Surv(time,status==2)~age+sex,vars=c("edema","bili","protime"),data=pbc)
##' controlled.hr
##' 
##' @export
coxphSeries <- function(formula,data,vars,...){
    ## ref <- glm(formula,data=data,...)
    Missing=NULL
    data.table::setDT(data)
    data <- data[,c(all.vars(formula),vars),with=FALSE]
    clist <- lapply(vars,function(v){
        form.v <- update.formula(formula,paste(".~.+",v))
        if (is.logical(data[[v]]))
            data[[v]] <- factor(data[[v]],levels=c("FALSE","TRUE"))
        cf <- survival::coxph(form.v,data=data,...)
        cf$call$data <- data
        cf$model <- data
        nv <- length(cf$xlevels[[v]])
        rtab <- regressionTable(cf)
        rtab[[v]]
    })
    out <- data.table::rbindlist(clist)
    if (all(out$Missing%in%c("","0")))
        out[,Missing:=NULL]
    out[]
}
