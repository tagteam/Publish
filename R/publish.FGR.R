publish.FGR <- function(x,digits=4,...){
  publish(summary(x$crrFit)$coef,digits=digits,...)
}
