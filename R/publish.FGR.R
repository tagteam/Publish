##' @author Thomas Alexander Gerds <tab@@biostat.ku.dk>
##' 
##' @export
publish.FGR <- function(object,digits=4,print=TRUE,...){
    sum <- summary(object$crrFit)$coef
    if (print==TRUE) publish(sum,digits=digits,...)
    invisible(sum)
}
