##' @author Thomas Alexander Gerds <tab@@biostat.ku.dk>
##' 
##' @export
publish.FGR <- function(object,digits=4,print=TRUE,...){
    sum <- summary(object$crrFit)
    p <- sum$coef[,5,drop=TRUE]
    subHR <- pubformat(sum$coef[,2,drop=TRUE],handler="sprintf",digits=digits)
    ci <- sum$conf.int[,3:4]
    colnames(ci) <- c("lower","upper")
    ci <- formatCI(x=subHR,
                   ci[,"lower"],
                   ci[,"upper"],
                   show.x=0L)
    out <- data.table::data.table(cbind(Variable=rownames(sum$coef),
                                        subHR,
                                        ci,
                                        p))
    if (print==TRUE) publish(out,digits=digits,...)
    invisible(out)
}
