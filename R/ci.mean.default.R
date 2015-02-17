##' Compute mean values with confidence intervals
##'
##' Normal approximation 
##' @title Compute mean values with confidence intervals
#' @param x numeric vector
#' @param alpha level of significance 
#' @param normal If \code{TRUE} use quantile of t-distribution else use normal approximation and quantile of normal approximation. Do you think this is confusing? 
#' @param na.rm If \code{TRUE} remove missing values from \code{x}.
#' @param statistic Decide which mean to compute: either \code{"arithmetic"} or \code{"geometric"}
#' @param ... not used
##' @return a list with mean values and confidence limits
##' @author Thomas Gerds
#' @export
ci.mean.default <- function(x,
                            alpha = 0.05,
                            normal = TRUE,
                            na.rm=TRUE,
                            statistic="arithmetic",...){
    stat <- match.arg(statistic,c("arithmetic","geometric"))
    if (na.rm){x <- x[!is.na(x)]}
    if (stat=="geometric") x <- log(x) 
    n <- length(x)
    m <- mean(x)
    se <- sqrt(var(x)/n)
    df <- n - 1
    if(normal) {
        q <- qt(1 - alpha/2, df)
    }
    else {
        q <- qnorm(1 - alpha/2)
    }
    low <- m - se * q
    up <- m + se * q
    if (stat=="geometric")
        out <- list(geomean = exp(m), se = exp(se),lower = exp(low), upper = exp(up), level=alpha, statistic=stat)
    else
        out <- list(mean = m, se = se,lower = low, upper = up, level=alpha, statistic=stat)
    class(out) <- c("ci",class(out))
    out
}
