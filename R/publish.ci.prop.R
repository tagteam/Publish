publish.ci.prop <- function(x,digits=2,percent=TRUE,style=2,...){
 e <- if(percent) round(100*x$estimate,digits) else round(x$estimate,digits)
 l <- if(percent) round(100*x$conf.int[1],digits) else round(x$conf.int[1],digits)
 u <- if(percent) round(100*x$conf.int[2],digits) else round(x$conf.int[2],digits)
 paste(e,format.ci(l,u,style=style,...))
}
