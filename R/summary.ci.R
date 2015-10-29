##' Summarize confidence intervals 
##'
##' This format of the confidence intervals is user-manipulable.
##' @title Summarize confidence intervals
##' @param object Object of class ci containing point estimates and the
##' corresponding confidence intervals
##' @param format A string which indicates the format used for
##' confidence intervals.  The string is passed to
##' \code{\link{formatCI}} with two arguments: the lower and the upper
##' limit. For example \code{'(l;u)'} yields confidence intervals with
##' round parenthesis in which the upper and the lower limits are
##' separated by semicolon.
##' @param se If \code{TRUE} add standard error.
##' @param print Logical: if \code{FALSE} do not actually print
##' confidence intervals but just return them invisibly.
##' @param ... used to control formatting of numbers
##' @return Formatted confidence intervals
##' @seealso ci plot.ci format.ci
##' @examples
##' library(lava)
##' m <- lvm(Y~X)
##' m <- categorical(m,Y~X,K=4)
##' set.seed(4)
##' d <- sim(m,24)
##' ci.mean(Y~X,data=d)
##' x <- summary(ci.mean(Y~X,data=d),digits=2)
##' x
##' x <- summary(ci.mean(Y~X,data=d),format="(u,l)",digits=2)
##' x <- summary(ci.mean(Y~X,data=d),format="(u,l)",digits=1,se=TRUE)
##' x <- summary(ci.mean(Y~X,data=d),format="(u,l)",digits=1,handler="format")
##' x <- summary(ci.mean(Y~X,data=d),format="(u,l)",digits=1,handler="prettyNum")
#' @export
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
summary.ci <- function(object,format="[u;l]",se=FALSE,print=TRUE,...){
    pynt <- getPyntDefaults(list(...),names=list("digits"=c(2,3),"handler"="sprintf",nsmall=NULL))
    digits <- pynt$digits
    handler <- pynt$handler
    if (length(digits)==1) digits <- rep(digits,2)
    if (length(pynt$nsmall)>0) nsmall <- pynt$nsmall else nsmall <- pynt$digits
    if (missing(format) || is.null(format)) format <- "[u;l]"
    if (is.null(object$level))   level <- 0.05 else level <- object$level
    parm <- pubformat(object[[1]],handler=handler,digits=digits,nsmall=nsmall)
    ci <- formatCI(lower=object[["lower"]],upper=object[["upper"]],format=format,handler=handler,digits=digits,nsmall=nsmall)
    if (match("se",names(object)) && se==TRUE){
        se <- pubformat(object[[2]],handler=handler,digits=digits,nsmall=nsmall)
        pci <- cbind(parm,se,ci)
        colnames(pci) <- c(names(object)[1:2],paste("CI-",as.character(100*(1-level)),"%",sep=""))
    }else{
        pci <- cbind(parm,ci)
        colnames(pci) <- c(names(object)[1],paste("CI-",as.character(100*(1-level)),"%",sep=""))
    }
    pci <- cbind(object$labels,pci)
    rownames(pci) <- rep("",nrow(pci))
    if (print==TRUE)
        print(pci,right=FALSE,quote=FALSE,...)
    invisible(pci)
}
