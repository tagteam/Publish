ci.geomean <- function(x,alpha = 0.05,normal = T,na.rm=T){
  if (na.rm){x <- x[!is.na(x)]}
  logx <- log(x)
  n <- length(logx)
  m <- mean(logx)
  se <- sqrt(var(logx)/n)
  df <- n - 1
  if(normal) {
    q <- qt(1 - alpha/2, df)
  }
  else {
    q <- qnorm(1 - alpha/2)
  }
  low <- m - se * q
  up <- m + se * q
  m <- exp(m)
  se <- exp(se)
  low <- exp(low)
  up <- exp(up)
  out <- data.frame(geomean = m,se = se,lower = low,upper = up)
  class(out) <- c("ci", class(out))
  out
}
