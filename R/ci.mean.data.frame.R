ci.mean.data.frame <- function(x,alpha = 0.05,normal = T,na.rm=T,statistic=c("arithmetic","geometric")){
  res <- lapply(x,ci.mean.default,alpha=alpha,normal=normal,na.rm=na.rm,statistic=statistic)
  tmp <- data.frame(t(sapply(t(res),function(x)unlist(x[1:4]))))
  tmp$labels <- names(x)
  out <- lapply(tmp,function(x)x)
  out <- c(out,level=alpha,statistic=statistic)
  class(out) <- c("ci",class(out))
  out
}
