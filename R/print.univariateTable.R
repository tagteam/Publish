##' Print function for univariate tables
##'
##' This function is simply calling \code{summary.univariateTable}
##' @title Printing univariate tables
##' @param x An object obtained with \code{univariateTable}
##' @param ... Passed to summary.univariateTable
##' @return The result of \code{summary.univariateTable(x)}
##' @seealso \code{univariateTable}
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
print.univariateTable <- function(x,...){
  sx <- summary(x,...)
  print(sx)
  invisible(sx)
}
