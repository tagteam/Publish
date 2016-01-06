### publish.ci.R --- 
#----------------------------------------------------------------------
## author: Thomas Alexander Gerds
## created: Oct 29 2015 (06:41) 
## Version: 
## last-updated: Dec 17 2015 (09:23) 
##           By: Thomas Alexander Gerds
##     Update #: 5
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
##' Publish tables with confidence intervals
##'
##' This function calls summary.ci with print=FALSE and then publish
##' @title Publish tables with confidence intervals
##' @param object Object of class ci containing point estimates and the
##' corresponding confidence intervals
##' @param format  A string which indicates the format used for
##' confidence intervals.  The string is passed to
##' \code{\link{formatCI}} with two arguments: the lower and the upper
##' limit. For example \code{'(l;u)'} yields confidence intervals with
##' round parenthesis in which the upper and the lower limits are
##' separated by semicolon.
##' @param se If \code{TRUE} add standard error.
##' @param ... passed to \code{publish}
##' @return table with confidence intervals
##' @seealso summary.ci
##' @examples
##'
##' data(Diabetes)
##' publish(ci.mean(chol~location+gender,data=Diabetes),org=TRUE)
##' 
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
publish.ci <- function(object,format="[u;l]",se=FALSE,...){
    publish(summary(object,se=se,format=format,print=FALSE),...)
}


#----------------------------------------------------------------------
### publish.ci.R ends here
