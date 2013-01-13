publish.prodlim <- function(object,times,intervals=TRUE,percent=TRUE,digits=ifelse(percent,1,3),cause=1,...){
  if (missing(times)) stop("Argument times is missing with no default.")
  so <- summary(object,times=times,intervals=intervals,percent=percent,cause=cause)$table
  if (length(so)==1) {
    so <- so[[1]]
    out <- so[,c("n.risk","n.event","n.lost")]
    colnames(out) <- c("No. at risk","No. of events","No. lost to follow-up")
    if (is.null(object$cuminc)){
      out <- cbind(out,"Survival probability"=round(so[,"surv"],digits))
    }
    else{
      out <- cbind(out,"Cumulative incidence"=round(so[,"cuminc"],digits))
    }
    out <- cbind("Interval"=apply(round(so[,c("time0","time1")],digits),1,paste,collapse="--"),
                 out,
                 "CI.95"=apply(round(so[,c("lower","upper")],digits),1,paste,collapse="--"))
    publish(out,rownames=FALSE,...)
  }
  else{
    names <- names(so)
    u <- lapply(1:length(so),function(i){
      x <- so[[i]]
      out <- x[,c("n.risk","n.event","n.lost")]
      colnames(out) <- c("No. at risk","No. of events","No. lost to follow-up")
      if (is.null(object$cuminc)){
        out <- cbind(out,"Survival probability"=round(x[,"surv"],digits))
      }
      else{
        out <- cbind(out,"Cumulative incidence"=round(x[,"cuminc"],digits))
      }
      out <- cbind("Interval"=apply(round(x[,c("time0","time1")],digits),1,paste,collapse="--"),
                   out,
                   "CI.95"=apply(round(x[,c("lower","upper")],digits),1,paste,collapse="--"))
      publish(names[i])
      publish(out,rownames=FALSE,...)})
  }
  invisible(u)
}
