### plot.regressionTable.R --- 
#----------------------------------------------------------------------
## author: Thomas Alexander Gerds
## created: Feb  2 2015 (06:55) 
## Version: 
## last-updated: Feb 17 2015 (16:31) 
##           By: Thomas Alexander Gerds
##     Update #: 12
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
##' 
##' @export
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
plot.regressionTable <- function(x,...){
    X <- summary(x)
    at <- attributes(X)
    Coef <- at$Coefficient
    Lower <- at$Lower
    Upper <- at$Upper
    plot.ci(list(Coef,lower=Lower,upper=Upper),
            plot.xlim=c(min(Lower),max(Upper)),
            labels=X$Variable,
            title.labels="Factor")
}


#----------------------------------------------------------------------
### plot.regressionTable.R ends here
