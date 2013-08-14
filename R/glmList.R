glmList <- function(formula,data,vars,...){
  glist <- lapply(vars,function(v){
    form.v <- update.formula(formula,paste(".~.+",v))
    gf <- glm(form.v,data=data,...)
    gf$call$data <- data
    u <- publish(gf,print=FALSE)
    u <- u[grep(v,rownames(u)),,drop=FALSE]
  })
  u <- sapply(glist,NCOL)
  if (any(v <- (u<max(u)))){
    for (x in (1:length(glist))[v]){
      glist[[x]] <- cbind(glist[[x]],data.frame("Missing"=rep("--",NROW(glist[[x]])),stringsAsFactors=FALSE))
    }
  }
  do.call("rbind",glist)
}
## glmList(bodyfat~1,data=Bodyfat,vars=c("Age","Weight","Height","Chest"))
