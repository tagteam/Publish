##' Format confidence intervals
##'
##' The default format for confidence intervals is [lower; upper].
##' @title Formatting confidence intervals
##' @param x not used (for compatibility with format)
##' @param lower Numeric vector of lower limits
##' @param upper Numeric vector of upper limits
##' @param show.x Logical. If \code{TRUE} show value of x in front of confidence interval.
##' @param handler Function to format numeric values. Default is
##'     \code{sprintf}, also supported are \code{format} and
##'     \code{prettyNum}
##' @param format Character string in which \code{l} will be replaced
##'     by the value of the lower limit (argument lower) and \code{u}
##'     by the value of the upper upper limit. For example,
##'     \code{(l,u)} yields confidence intervals in round parenthesis
##'     in which the upper and lower limits are comma
##'     separated. Default is \code{[l;u]}.
##' @param degenerated String to show when lower==upper.  Default is
##'     '--'
##' @param digits If handler \code{format} or \code{prettyNum} used
##'     format numeric vectors.
##' @param nsmall If handler \code{format} or \code{prettyNum} used
##'     format numeric vectors.
##' @param sep Field separator
##' @param reference.pos Position of factor reference  
##' @param reference.label Label for factor reference
##' @param ... passed to handler
##' @return String vector with confidence intervals
##' @seealso plot.ci ci.mean
##' @examples
##'
##' x=ci.mean(rnorm(10))
##' formatCI(lower=x[3],upper=x[4])
##' formatCI(lower=c(0.001,-2.8413),upper=c(1,3.0008884))
##' # change format
##' formatCI(lower=c(0.001,-2.8413),upper=c(1,3.0008884),format="(l, u)")
##' # show x
##' formatCI(x=x$mean,lower=x$lower,upper=x$upper,format="(l, u)",show.x=TRUE)
##'
##' # change of handler function
##' l <- c(-0.0890139,0.0084736,144.898333,0.000000001)
##' u <- c(0.03911392,0.3784706,3338944.8821221,0.00001)
##' cbind(format=formatCI(lower=l,upper=u,format="[l;u)",digits=2,nsmall=2,handler="format"),
##'       prettyNum=formatCI(lower=l,upper=u,format="[l;u)",digits=2,nsmall=2,handler="prettyNum"),
##'       sprintf=formatCI(lower=l,upper=u,format="[l;u)",digits=2,nsmall=2,handler="sprintf"))
##'
##' @export
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
formatCI <- function(x,
                     lower,
                     upper,
                     show.x=FALSE,
                     handler="sprintf",
                     format="[l;u]",
                     degenerated="asis",
                     digits=2,
                     nsmall=digits,
                     sep="",
                     reference.pos,
                     reference.label="",
                     ...){
    stopifnot(length(upper)==length(lower))
    format <- sub("l","%s",format)
    format <- sub("u","%s",format)
    lower <- pubformat(lower,digits=digits[[1]],nsmall=nsmall[[1]],handler=handler)
    upper <- pubformat(upper,digits=digits[[1]],nsmall=nsmall[[1]],handler=handler)
    N <- length(lower)
    out <- sapply(1:N,function(i){
        if (is.character(degenerated) && degenerated!="asis" && lower[i]==upper[i])
            ci <- degenerated
        else
            ci <- do.call("sprintf",list(fmt=format,lower[i],upper[i]))
        ci
    })
    if (show.x)
        out <- paste(pubformat(x,digits=digits,handler=handler,nsmall=nsmall),out)
    if (!missing(reference.pos))
        out[reference.pos] <- reference.label
    out
}
