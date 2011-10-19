publish.crossTable <- function(x,trans=FALSE,...){
  if (trans) {
    x <- t(x)
    colnames(x) <- x[1,]
    publish(x[-1,],...)
  }
  else{
    NextMethod("publish",x,...)
  }
}
