##' @export
publish.pec <- function(object,times, digits=3,what=NULL,...){
  # FROM PRINT.PEC
  if (is.null(what))
    if (object$method$internal.name=="noPlan")
      what="AppErr"
    else
      what <- paste(object$method$internal.name,"Err",sep="")
  if (missing(times)){
    ## times <- object$maxtime ## times <- quantile(object$time,.9)
    times <- object$minmaxtime
    ##     naPos <- sapply(object[[what]],function(pec){
    ##       length(pec)-sum(is.na(pec))-1
    ##     })
    ##     times <- min(object$time[naPos],times)
  }
  # {{{ cumulative prediction errors
  tnames <- if(times<1) signif(times,2) else round(times,2)
  out <- pec::crps(object=object,
              times=times,
              start=object$start,
              what=what)
  publish(out,digits=digits,quote=FALSE)
  # }}}
}
  
