### plot.regressionTable.R ---
#----------------------------------------------------------------------
## author: Thomas Alexander Gerds
## created: Feb  2 2015 (06:55)
## Version:
## last-updated: Mar  7 2018 (09:18) 
##           By: Thomas Alexander Gerds
##     Update #: 101
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
##' @param style Determines how to arrange variable names and their corresponding units 
##' @param ... passed to plotConfidence
##' @return NULL
##' @seealso regressionTable
##' @examples
##' ## linear regression
##' data(Diabetes)
##' f <- glm(bp.1s~AgeGroups+chol+gender+location,data=Diabetes)
##' rtf <- regressionTable(f,factor.reference = "inline")
##' plot(rtf,cex=1.3)
##' 
##' ## logistic regression
##' data(Diabetes)
##' f <- glm(I(BMI>25)~bp.1s+AgeGroups+chol+gender+location,data=Diabetes,family=binomial)
##' rtf <- regressionTable(f,factor.reference = "inline")
##' plot(rtf,cex=1.3)
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
##' plot(pubcox,cex=1.5,xratio=c(0.4,0.2))
##'
##' @export
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
plot.regressionTable <- function(x,xlim,xlab,style=1,...){
    plot(summary(x,print=FALSE),xlim=xlim,xlab=xlab,style=style,...)
}
##' @export
plot.summary.regressionTable <- function(x,xlim,xlab,style=1,...){
    X <- x$rawTable
    X <- labelUnits(X,...)
    if (sum(X$Units=="")>0)
        X[X$Units=="",]$Units <- "1 unit"
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
    V <- X$Variable
    if (style==1){
        Labs <- split(U,rep(1:length(x$blocks),x$blocks))
        names(Labs) <- names(x$blocks)
        labels <- list(...)
        keys <- names(labels)
        Flabels <- labels[match(keys,names(Labs),nomatch=0)!=0]
        if (length(Flabels)>0)
        names(Labs)[match(keys,names(Labs),nomatch=0)] <- Flabels
    } else {
        Labs <- data.frame(Variable=V,Units=U)
    }
    plotConfidence(list(Coef,lower=Lower,upper=Upper),
                   xlim=xlim,
                   labels=Labs,
                   xlab=xlab,
                   refline=1*(model!="Linear regression"),
                   ...)
}


#----------------------------------------------------------------------
### plot.regressionTable.R ends here
