CoxList <- function(formula,data,vars,print=FALSE,...){
  clist <- lapply(vars,function(v){
    form.v <- update.formula(formula,paste(".~.+",v))
    cf <- coxph(form.v,data=data)
    cf$call$data <- data
    u <- publish(cf,print=print,missing=TRUE,...)
    u <- u[grep(v,rownames(u)),]
  })
  u <- sapply(clist,NCOL)
  if (any(v <- (u<max(u)))){
    for (x in (1:length(clist))[v]){
      clist[[x]] <- cbind(clist[[x]],data.frame("Missing"=rep("--",NROW(clist[[x]])),stringsAsFactors=FALSE))
    }
  }
  do.call("rbind",clist)
}
