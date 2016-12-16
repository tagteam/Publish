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
  clist <- lapply(vars,function(v){
    form.v <- update.formula(formula,paste(".~.+",v))
    cf <- survival::coxph(form.v,data=data,...)
    cf$call$data <- data
    u <- summary(regressionTable(cf),print=FALSE)$regressionTable
    u <- u[grep(v,u$Variable),]
  })
  u <- sapply(clist,NCOL)
  if (any(v <- (u<max(u)))){
    for (x in (1:length(clist))[v]){
      clist[[x]] <- cbind(clist[[x]],data.frame("Missing"=rep("--",NROW(clist[[x]])),stringsAsFactors=FALSE))
    }
  }
  do.call("rbind",clist)
}
