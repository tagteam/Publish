ci.prop <- function(x,n,...){
  out <- binom.test(x,n,...)
  class(out) <- "ci.prop"
  out
}
  
