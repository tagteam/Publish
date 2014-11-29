##' @S3method publish univariateTable
publish.univariateTable <- function(object,...){
    publish(summary(object,...),...)
}
