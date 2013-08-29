publish.univariateTable <- function(x,missing=c("ifany","always","never"),order,...){
  publish(summary(x,missing=missing,order=order),...)
}
