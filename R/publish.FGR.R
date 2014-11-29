##' @S3method publish FGR
publish.FGR <- function(object,digits=4,print=TRUE,...){
    sum <- summary(object$crrFit)$coef
    if (print==TRUE) publish(sum,digits=digits,...)
    invisible(sum)
}
