iqr <- function (x, na.rm = FALSE,digits,...){
  paste("[",paste(round(quantile(as.numeric(x), c(0.25, 0.75), na.rm = na.rm),digits),collapse=","),"]",sep="")
}
