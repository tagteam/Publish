##' @S3method publish BaselineTable
publish.BaselineTable <- function(x,order,...){
  publish(summary(x),...)
}
