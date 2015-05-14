### plot.regressionTable.R ---
#----------------------------------------------------------------------
## author: Thomas Alexander Gerds
## created: Feb  2 2015 (06:55)
## Version:
## last-updated: May 14 2015 (10:41) 
##           By: Thomas Alexander Gerds
##     Update #: 22
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
##' @param ... passed to plot.ci
##' @return NULL
##' @seealso regressionTable
##' @examples
##'
##' data(Diabetes)
##' f <- glm(bp.2s~bp.1s+age+chol+gender+location,data=Diabetes)
##' rtf <- regressionTable(f,factor.reference = "inline")
##' plot(rtf,cex=2,xratio=c(0.2,0.3))
##'
##' data(trace)
##' fit<-glm(dead ~ smoking+ sex+ age+Time+offset(log(ObsTime)), family = poisson,data=trace)
##' fit2<- regressionTable(fit,factor.reference = "inline")
##' plot(fit2,xlim=c(0.85,1.15),xratio=c(0.2,0.3),cex=2)
##'
##'
##' @export
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
plot.regressionTable <- function(x,xlim,...){
    X <- do.call("rbind",x)
    Coef <- X[,grep("OddsRatio|HazardRatio|Coefficient",colnames(X))]
    Lower <- X$Lower
    Upper <- X$Upper
    if (missing(xlim)) xlim <- c(min(Lower),max(Upper))
    plotConfidence(list(Coef,lower=Lower,upper=Upper),
                   xlim=xlim,
                   labels=list("Variable"=X$Variable),...)
}


#----------------------------------------------------------------------
### plot.regressionTable.R ends here
