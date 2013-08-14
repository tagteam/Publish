publish.ci.prop <- function(x,digits=2,percent=TRUE,style=2,...){
 e <- if(percent) format(100*x$estimate,digits=digits,nsmall=digits) else format(x$estimate,digits=digits,nsmall=digits)
 l <- if(percent) format(100*x$conf.int[1],digits=digits,nsmall=digits) else format(x$conf.int[1],digits=digits,nsmall=digits)
 u <- if(percent) format(100*x$conf.int[2],digits=digits,nsmall=digits) else format(x$conf.int[2],digits=digits,nsmall=digits)
 paste(e,format.ci(l,u,style=style,...))
}
