plot.ci <- function(ci,ci.pch=16,ci.lwd=1.3,horizontal=TRUE,...){
  m <- ci[[1]]
  lower <- ci$lower
  upper <- ci$upper
  len <- length(m)
  if (horizontal){
    ## die erste zeile soll oben sein
    plot(m,len:1,ylim=c(0,len+1),pch=ci.pch,...)
    segments(lower,len:1,upper,len:1,lwd=ci.lwd)
  }
  else{
    ## die erste spalte soll links sein
    plot(1:len,m,xlim=c(0,len+1),pch=ci.pch,...)
    segments(y0=lower,x0=1:len,y1=upper,x1=1:len,lwd=ci.lwd)
  }
}
