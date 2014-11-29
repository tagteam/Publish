##' @S3method publish ci.prop
publish.ci.prop <- function(object,digits=2,percent=TRUE,format="[l;u]",...){
 e <- if(percent) format(100*object$estimate,digits=digits,nsmall=digits) else format(object$estimate,digits=digits,nsmall=digits)
 l <- if(percent) format(100*object$conf.int[1],digits=digits,nsmall=digits) else format(object$conf.int[1],digits=digits,nsmall=digits)
 u <- if(percent) format(100*object$conf.int[2],digits=digits,nsmall=digits) else format(object$conf.int[2],digits=digits,nsmall=digits)
 paste(e,formatCI(l,u,format=format,...))
}
