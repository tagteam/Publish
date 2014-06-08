##' @S3method publish univariateTable
publish.univariateTable <- function(x,...){
    publish(summary(x,...),...)
}
