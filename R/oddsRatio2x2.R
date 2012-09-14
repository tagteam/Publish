oddsRatio2x2 <- function(tab){
  a <- tab[1,1]
  b <- tab[1,2]
  c <- tab[2,1]
  d <- tab[2,2]
  alpha <- 0.05
  or <- (a*d)/(b*c)
  or.lower <- exp(log(or) - qnorm(1-alpha/2)*sqrt(1/a+1/b+1/c+1/d))
  or.upper <- exp(log(or) + qnorm(1-alpha/2)*sqrt(1/a+1/b+1/c+1/d))
  p <- chisq.test(tab)$p.value
  list(or=or,lower=or.lower,upper=or.upper,pvalue=p)
}
