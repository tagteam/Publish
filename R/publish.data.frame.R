##' @S3method publish data.frame
publish.data.frame <- function(x,...){
  publish(as.matrix(x),...)
}
