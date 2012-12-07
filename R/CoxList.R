CoxList <- function(formula,data,vars){
  clist <- lapply(vars,function(v){
    form.v <- update.formula(formula,paste(".~.+",v))
    cf <- coxph(form.v,data=data)
    cf$call$data <- data
    u <- publish(cf,print=F)
    u <- u[grep(v,rownames(u)),]
  })
  do.call("rbind",clist)
}
