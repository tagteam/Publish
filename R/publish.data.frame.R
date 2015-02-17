##' @export
publish.data.frame <- function(object,...){
  publish(as.matrix(object),...)
}
