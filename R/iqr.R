iqr <- function (x, na.rm = FALSE,digits,...){
  paste("[",paste(format(quantile(as.numeric(x), c(0.25, 0.75), na.rm = na.rm),digits=digits,nsmall=digits),collapse=","),"]",sep="")
}
