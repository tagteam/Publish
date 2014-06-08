##' Run a series of generalized linear regression analyses and summarize the results in a table
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
##' @param ... passed to coxph
##' @return Matrix with regression coefficients, one for each element of \code{vars}.
##' @author Thomas Alexander Gerds
##'
##' ##' @export
glmSeries <- function(formula,data,vars,...){
    ## ref <- glm(formula,data=data,...)
    glist <- lapply(vars,function(v){
        form.v <- update.formula(formula,paste(".~.+",v))
        if (is.logical(data[,v]))
            data[,v] <- factor(data[,v],levels=c("FALSE","TRUE"))
        gf <- glm(form.v,data=data,...)
        gf$call$data <- data
        nv <- length(gf$xlevels[[v]])
        u <- publish(gf,print=FALSE)
        first <- grep(v,u[,"Variable"])
        if (nv>1)
            sel <- seq(first,first+nv-1,1)
        else
            sel <- first
        u <- u[sel,,drop=FALSE]
        u
    })
    u <- sapply(glist,NCOL)
    if (any(v <- (u<max(u)))){
        for (x in (1:length(glist))[v]){
            glist[[x]] <- cbind(glist[[x]],data.frame("Missing"=rep("--",NROW(glist[[x]])),stringsAsFactors=FALSE))
        }
    }
    do.call("rbind",glist)
}
