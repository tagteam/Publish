smart.ciplot.formula <- function(formula,
                                 data,
                                 fun="ci.mean.formula",
                                 fun.args=list(statistic="arithmetic"),
                                 print.ci=TRUE,
                                 horizontal=FALSE,
                                 las=ifelse(horizontal,1,2),
                                 srt=ifelse(horizontal,0,90),
                                 cex.labels,
                                 sep.lty,
                                 xlab="horizontal.lab",
                                 ylab="vertical.lab",
                                 lab.pos,
                                 group.names,
                                 plot.group.names=TRUE,
                                 offset,
                                 ...){
  if (horizontal) ylab <- "" else xlab <- ""
  ci <- do.call(fun,c(list(formula=formula,data=data,alpha=0.05),fun.args))
  if (print.ci) print.ci(ci)
  plot.ci(ci,ylab=ylab,xlab=xlab,horizontal=horizontal,axes=FALSE,...)
  lab <- data.frame(ci$labels)
  if (!missing(group.names)) names(lab) <- group.names
  if (missing(sep.lty)) sep.lty <- rep(0,c(ncol(ci$labels)))
  smart.labels(labels=lab,rev=TRUE,sep.lty=sep.lty,cex.labels=cex.labels,pos=lab.pos,srt=srt,offset=offset,groupnames=plot.group.names,side=as.numeric(!horizontal))
  axis(ifelse(horizontal,1,2))
}
