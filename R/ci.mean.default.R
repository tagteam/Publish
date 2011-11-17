ci.mean.default <- function(x,
                            alpha = 0.05,
                            normal = TRUE,
                            na.rm=TRUE,
                            statistic="arithmetic"){
  stat <- match.arg(statistic,c("arithmetic","geometric"))
  if (na.rm){x <- x[!is.na(x)]}
  if (stat=="geometric") x <- log(x) 
  n <- length(x)
  m <- mean(x)
  se <- sqrt(var(x)/n) 
  df <- n - 1
  if(normal) {
    q <- qt(1 - alpha/2, df)
  }
  else {
    q <- qnorm(1 - alpha/2)
  }
  low <- m - se * q
  up <- m + se * q
  if (stat=="geometric")
    out <- list(geomean = exp(m), se = exp(se),lower = exp(low), upper = exp(up), level=alpha, statistic=stat)
  else
    out <- list(mean = m, se = se,lower = low, upper = up, level=alpha, statistic=stat)
  class(out) <- c("ci",class(out))
  out
}
