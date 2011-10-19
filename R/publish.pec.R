publish.pec <- function(x,times, digits=3,what=NULL,...){
  # FROM PRINT.PEC
  if (is.null(what))
    if (x$method$internal.name=="noPlan")
      what="AppErr"
    else
      what <- paste(x$method$internal.name,"Err",sep="")
  if (missing(times)){
    ## times <- x$maxtime ## times <- quantile(x$time,.9)
    times <- x$minmaxtime
    ##     naPos <- sapply(x[[what]],function(pec){
    ##       length(pec)-sum(is.na(pec))-1
    ##     })
    ##     times <- min(x$time[naPos],times)
  }
  # {{{ cumulative prediction errors
  tnames <- if(times<1) signif(times,2) else round(times,2)
  out <- crps(object=x,
              times=times,
              start=x$start,
              what=what)
  publish(out,digits=digits,quote=FALSE)
  # }}}
}
  
