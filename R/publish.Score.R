### publish.Score.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Jun 10 2017 (17:47) 
## Version: 
## Last-Updated: Dec  1 2020 (16:49) 
##           By: Thomas Alexander Gerds
##     Update #: 17
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
##' Write output of \code{riskRegression::Score} in tables
##'
##' Collect prediction accuracy results in tables
##' @title Publish predictive accuracy results
##' @param object Object obtained with \code{riskRegression::Score}
##' @param metrics Which metrics to put into tables. Defaults to
##'     \code{object$metrics}.
##' @param score Logical. If \code{TRUE} print the score elements, i.e., metric applied to the risk prediction models. 
##' @param contrasts Logical. If \code{TRUE} print the contrast elements (if any). These compare risk prediction models according to metrics. 
##' @param level Level of subsection headers, i.e., ** for level 2 and
##'     *** for level 3 (useful for emacs org-users). Default is plain
##'     subsection headers no stars.  A negative value will suppress
##'     subjection headers.
##' @param ... Passed to publish
##' @return Results of Score in tabular form
##' @examples
##' if (requireNamespace("riskRegression",quietly=TRUE)){
##' library(riskRegression)
##' library(survival)
##' learn = sampleData(100)
##' val= sampleData(100)
##' f1=CSC(Hist(time,event)~X1+X8,data=learn)
##' f2=CSC(Hist(time,event)~X1+X5+X6+X8,learn)
##' xs=Score(list(f1,f2),data=val,formula=Hist(time,event)~1)
##' publish(xs)
##' }
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
publish.Score <- function(object,metrics,score=TRUE,contrasts=TRUE,level=3,...){
    if (missing(metrics)) metrics <- object$metrics
    for (m in metrics){
        if (level>0){
            publish(paste0("Metric ",m,":\n"),level=level,...)
            publish("Assessment of predictive accuracy",level=level+1)
        }
        if (score){
            publish(object[[m]]$score, ...)
        }
        if (contrasts && !is.null(object[[m]]$contrasts)){
            if (level>0){
                org("Comparison of predictive accuracy",level=level+1)
            }
            publish(object[[m]]$contrasts, ...)
        }
    }
}


######################################################################
### publish.Score.R ends here
