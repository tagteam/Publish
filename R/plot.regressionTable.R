### plot.regressionTable.R ---
#----------------------------------------------------------------------
## author: Thomas Alexander Gerds
## created: Feb  2 2015 (06:55)
## Version:
## last-updated: Mar 31 2015 (15:06)
##           By: Thomas Alexander Gerds
##     Update #: 18
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
##' plot(rtf)
##'
##' data(trace)
##' fit<-glm(dead ~ smoking+ sex+ age+Time+offset(log(ObsTime)), family = poisson,data=trace)
##' fit2<- regressionTable(fit,factor.reference = "inline")
##' plot(fit2)
##'
##'
##' @export
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
plot.regressionTable <- function(x,...){
    X <- do.call("rbind",x)
    Coef <- X[,grep("OddsRatio|HazardRatio|Coefficient",colnames(X))]
    Lower <- X$Lower
    Upper <- X$Upper
    plot.ci(list(Coef,lower=Lower,upper=Upper),
            plot.xlim=c(min(Lower),max(Upper)),
            labels=X$Variable,...)
}


#----------------------------------------------------------------------
### plot.regressionTable.R ends here
