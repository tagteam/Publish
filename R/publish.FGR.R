publish.FGR <- function(x,digits=4,print=TRUE,...){
    sum <- summary(x$crrFit)$coef
    if (print==TRUE) publish(sum,digits=digits,...)
    invisible(sum)
}
