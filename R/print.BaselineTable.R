print.BaselineTable <- function(x,order,...){
  sx <- summary(x)
  print(sx)
  invisible(sx)
}
