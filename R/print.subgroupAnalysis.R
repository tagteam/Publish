##' Print function for subgroupAnalysis
##'
##' This function is simply calling \code{summary.subgroupAnalysis}
##' @title Printing univariate tables
##' @param x - An object obtained with \code{subgroupAnalysis}
##' @param ... Passed to summary.subgroupAnalysis
##' @return The result of \code{summary.subgroupAnalysis(x)}
##' @seealso \code{subgroupAnalysis}
##' @export 
##' @author Christian Torp-Pedersen (ctp@heart.dk)
print.subgroupAnalysis <- function(x,...){
  sx <- summary(x,...)
  print(sx)
  invisible(sx)
}
