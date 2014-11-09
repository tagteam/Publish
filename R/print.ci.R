##' Print confidence intervals
##'
##' This format of the confidence intervals is user-manipulable.
##' @title Print confidence intervals
##' @param x Object containing point estimates and the corresponding
##' confidence intervals
##' @param digits Digits for round values
##' @param format This argument is passed to \code{\link{format.ci}}
##' @param se
##' @param print Logical: if \code{FALSE} do not actually print
##' confidence intervals but just return them invisibly.
##' @param ... passed to print
##' @param style Style of confidence intervals
##' @return Formatted according to style
##' @seealso ci plot.ci format.ci
##' @examples
##' m <- lvm(Y~X)
##' m <- categorical(m,Y~X,K=4)
##' set.seed(4)
##' d <- sim(m,24)
##' ci.mean(Y~X,data=d)
##' x <- ci.mean(Y~X,data=d)
##' print(x,format="(l,u)")
 
#' @S3method print ci
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
print.ci <- function(x,digits=3,format="[l;u]",se=FALSE,print=TRUE,...){
    if (is.null(x$level))   level <- 0.05 else level <- x$level
    if (match("se",names(x)) && se==TRUE){
        pci <- cbind(format(cbind(x[[1]],x[[2]]),digits=digits,nsmall=digits),
                     apply(cbind(x[["lower"]],x[["upper"]]),
                           1,
                           function(x)format.ci(lower=x[1],upper=x[2],format=format,digits=digits)))
        colnames(pci) <- c(names(x)[1:2],paste("CI-",as.character(100*(1-level)),"%",sep=""))
    }else{
        pci <- cbind(format(cbind(x[[1]]),,digits=digits,nsmall=digits),
                     apply(cbind(x[["lower"]],x[["upper"]]),1,function(x){
                         format.ci(lower=x[1],upper=x[2],format=format,digits=digits)
                     }))
        colnames(pci) <- c(names(x)[1],paste("CI-",as.character(100*(1-level)),"%",sep=""))
    }
    pci <- cbind(x$labels,pci)
    rownames(pci) <- rep("",nrow(pci))
    if (print==TRUE)
        print(pci,right=FALSE,digits=digits,quote=FALSE,...)
    invisible(pci)
}
