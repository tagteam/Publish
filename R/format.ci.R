##' Format confidence intervals
##'
##' The default format for confidence intervals is [lower; upper]. 
##' @title Formatting confidence intervals
##' @param lower Vector of lower limits
##' @param upper Vector of upper limits
##' @param format Character string in which 'l' will be replaced by
##' the value lower and 'u' by the the value of upper. E.g., '(u,l)'
##' will give confidence intervals as round parenthesis in which the
##' limits are comma separated. Default is '[u;l]'.
##' @param degenerated String to show when lower==upper.  Default is
##' '--'
##' @param digits For rounding of values passed to format
##' @param nsmall For rounding of values passed to format
##' @param trim This argument is passed to format
##' @param sep
##' @return String vector with confidence intervals
##' @seealso plot.ci ci.mean
##' @examples
##' format.ci(lower=c(0.001,-2.8413),upper=c(1,3.0008884))
##' # change format
##' format.ci(lower=c(0.001,-2.8413),upper=c(1,3.0008884),format="(l, u)")
##' 
##' # the first lower limit is shorter than the second, to align numbers
##' # use trim:
##' format.ci(lower=c(0.001,-2.8413),upper=c(1,3.0008884),format="l--u",trim=FALSE)
##' 
##' @S3method format ci
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
format.ci <- function(lower,
                      upper,
                      format="[l;u]",
                      ## degenerated="--",
                      degenerated="asis",
                      digits=3,
                      nsmall=digits,
                      trim=TRUE,
                      sep=""){
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
