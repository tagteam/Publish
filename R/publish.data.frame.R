##' @method publish data.frame
publish.data.frame <- function(object,...){
  publish(as.matrix(object),...)
}
