##' Format confidence intervals
##'
##' The default format for confidence intervals is [lower; upper]. 
##' @title Formatting confidence intervals
##' @param x not used (for compatibility with format)
##' @param lower Vector of lower limits
##' @param upper Vector of upper limits
##' @param format Character string in which 'l' will be replaced by
##' the value of the lower limit (argument lower) and 'u' by the value
##' of the upper upper limit. For example, \code{'(l,u)'} yields
##' confidence intervals in round parenthesis in which the upper and lower
##' limits are comma separated. Default is '[l;u]'.
##' @param degenerated String to show when lower==upper.  Default is
##' '--'
##' @param digits For rounding of values passed to format
##' @param nsmall For rounding of values passed to format
##' @param trim This argument is passed to format
##' @param sep Field separator
##' @param ... not used
##' @return String vector with confidence intervals
##' @seealso plot.ci ci.mean
##' @examples
##' x=ci.mean(rnorm(10))
##' format(x)
##' formatCI(lower=c(0.001,-2.8413),upper=c(1,3.0008884))
##' # change format
##' formatCI(lower=c(0.001,-2.8413),upper=c(1,3.0008884),format="(l, u)")
##' 
##' # the first lower limit is shorter than the second, to align numbers
##' # use trim:
##' formatCI(lower=c(0.001,-2.8413),upper=c(1,3.0008884),format="l--u",trim=FALSE)
##' 
##' @export
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
formatCI <- function(x,
                     lower,
                     upper,
                     format="[l;u]",
                     degenerated="asis",
                     digits=3,
                     nsmall=digits,
                     trim=TRUE,
                     sep="",...){
    stopifnot(length(upper)==length(lower))
    format <- sub("l","%s",format)
    format <- sub("u","%s",format)
    lower <- format(lower,digits=digits,nsmall=digits,trim=trim)
    upper <- format(upper,digits=digits,nsmall=digits,trim=trim)
    N <- length(lower)
    out <- sapply(1:N,function(i){
        if (is.character(degenerated) && degenerated!="asis" && lower[i]==upper[i])
            ci <- degenerated
        else
            ci <- do.call("sprintf",list(fmt=format,lower[i],upper[i]))
        ci
    })
    out
}
