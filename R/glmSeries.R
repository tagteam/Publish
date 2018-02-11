##' Run a series of generalized linear regression analyses for a list of predictor variables
##' and summarize the results in a table.
##' The regression models can be adjusted for a fixed set of covariates.
##'
##' @title Run a series of generalized linear regression analyses
##' @param formula The fixed part of the regression formula. For
##' univariate analyses this is simply \code{y~1} where \code{y} is
##' the outcome variable. When the aim is to control the effect of
##' \code{vars} in each element of the series by a fixed set of
##' variables it is \code{y~x1+x2} where again y is the outcome and x1
##' and x2 are confounders.
##' @param data A \code{data.frame} in which we evaluate the formula.
##' @param vars A list of variable names, the changing part of the
##' regression formula.
##' @param ... passed to glm
##' @return Matrix with regression coefficients, one for each element of \code{vars}.
##' @author Thomas Alexander Gerds
##' @examples
##'
##' data(Diabetes)
##' Diabetes$hyper1 <- factor(1*(Diabetes$bp.1s>140))
##' ## collect odds ratios from three univariate logistic regression analyses
##' uni.odds <- glmSeries(hyper1~1,vars=c("chol","hdl","location"),data=Diabetes,family=binomial)
##' uni.odds
##' ## control the logistic regression analyses for age and gender
##' ## but collect only information on the variables in `vars'.
##' controlled.odds <- glmSeries(hyper1~age+gender,
##'                              vars=c("chol","hdl","location"),
##'                              data=Diabetes, family=binomial)
##' controlled.odds
##' @export
glmSeries <- function(formula,data,vars,...){
    ## ref <- glm(formula,data=data,...)
    Missing=NULL
    data.table::setDT(data)
    data <- data[,c(all.vars(formula),vars),with=FALSE]
    glist <- lapply(vars,function(v){
        form.v <- update.formula(formula,paste(".~.+",v))
        if (is.logical(data[[v]]))
            data[[v]] <- factor(data[[v]],levels=c("FALSE","TRUE"))
        gf <- glm(form.v,data=data,...)
        ## gf$call$data <- data
        gf$model <- data
        nv <- length(gf$xlevels[[v]])
        rtab <- regressionTable(gf)
        rtab[[v]]
    })
    out <- data.table::rbindlist(glist)
    if (all(out$Missing%in%c("","0")))
        out[,Missing:=NULL]
    out[]
}
