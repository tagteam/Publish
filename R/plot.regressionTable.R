### plot.regressionTable.R ---
#----------------------------------------------------------------------
## author: Thomas Alexander Gerds
## created: Feb  2 2015 (06:55)
## Version:
## last-updated: Oct 22 2017 (16:43) 
##           By: Thomas Alexander Gerds
##     Update #: 55
#----------------------------------------------------------------------
##
### Commentary:
##
### Change Log:
#----------------------------------------------------------------------
##
### Code:
##' Plotting regression coefficients with confidence limits
##'
##'
##' @title Plotting regression coefficients with confidence limits
##' @param x regression table obtained with regressionTable
##' @param xlim Limits for x-axis
##' @param xlab Label for x-axis
##' @param ... passed to plotConfidence
##' @return NULL
##' @seealso regressionTable
##' @examples
##'
##' ## logistic regression
##' data(Diabetes)
##' f <- glm(bp.2s~bp.1s+age+chol+gender+location,data=Diabetes)
##' rtf <- regressionTable(f,factor.reference = "inline")
##' plot(rtf,cex=2)
##'
##' ## Poisson regression
##' data(trace)
##' fit <- glm(dead ~ smoking+ sex+ age+Time+offset(log(ObsTime)), family = poisson,data=trace)
##' rtab <- regressionTable(fit,factor.reference = "inline")
##' plot(rtab,xlim=c(0.85,1.15),cex=1.8,xaxis.cex=1.5)
##'
##' ## Cox regression
##' library(survival)
##' data(pbc)
##' coxfit <- coxph(Surv(time,status!=0)~age+log(bili)+log(albumin)+factor(edema)+sex,data=pbc)
##' pubcox <- publish(coxfit)
##' plot(pubcox,cex=1.5)
##'
##' @export
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
plot.regressionTable <- function(x,xlim,xlab,...){
    plot(summary(x,print=FALSE),xlim=xlim,xlab=xlab,...)
}
##' @export
plot.summary.regressionTable <- function(x,xlim,xlab,...){
    X <- x$rawTable
    model <- x$model
    if (missing(xlab))
        xlab <- switch(model,
                       "Linear regression"="Difference",
                       "Logistic regression"="Odds ratio",
                       "Poisson regression"="Hazard ratio",
                       "Cox regression"="Hazard ratio")
    Coef <- X[,grep("OddsRatio|HazardRatio|ProbIndex|Coefficient",colnames(X))]
    Lower <- X$Lower
    Upper <- X$Upper
    if (missing(xlim)) xlim <- c(min(Lower),max(Upper))
    U <- X$Units
    if (any(U!=""))
        Labs <- data.frame(Variable=X$Variable,Units=U)
    else
        Labs <- data.frame(Variable=X$Variable)
    plotConfidence(list(Coef,lower=Lower,upper=Upper),
                   xlim=xlim,
                   labels=Labs,xlab=xlab,...)
}


#----------------------------------------------------------------------
### plot.regressionTable.R ends here
