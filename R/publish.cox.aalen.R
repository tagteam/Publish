publish.cox.aalen <- function(object,
                              conf.int = 0.95,
                              robust = FALSE,
                              scale=1,
                              digits = 2,
                              pvalDigits=4,
                              eps=.0001,
                              mark.pval=FALSE,
                              print=TRUE,
                              ...){
  # FIXME: DELETE THE PRINT OF COEF(OBJECT)
  coef.matrix <- coef(object)
  # ----------------------time-constant variables----------------------
  beta <- coef.matrix[ ,"Coef."]
  beta <- beta * scale
  if (robust) se <- coef.matrix[ ,"Robust SE"]
   else se <- coef.matrix[ ,"SE"]
  se <- se * scale
  # conf int constant
  z <- qnorm((1 + conf.int)/2, 0, 1)
  # nice variable names
  const.name <- rownames(coef.matrix)
  get.names <- substr(const.name,6,nchar(const.name))
  get.names <- strsplit(get.names,")")
  covNames <- sapply(1:length(get.names), function(x){
    if (length(get.names[[x]]) ==1) get.names[[x]]
    else paste(get.names[[x]][1], get.names[[x]][2], sep=":")
  })
  # p-values
  pVal <- 1 - pchisq((beta/se)^2, 1)
  pValue <- sapply(pVal,format.pval,digits=pvalDigits,eps=eps)
  if (mark.pval){
    pValue <- sapply(1: length(pVal), function(p){
      if (pVal[p] < 1-conf.int) paste("**",pValue[p],"**",sep="") 
      else pValue[p]
    })    
  }
  out <- data.frame("Var"=covNames,
               "HR"=format(round(exp(beta),digits)),
               "Std.err"=format(round(se,digits)),
               "CI.95"=paste("[",format(round(exp(beta - z * se),2)),";",format(round(exp(beta + z * se),2)),"]",sep=""),
               "P-value"=pValue)
  rownames(out) <- rep("",NROW(out))
  if (print==TRUE)
    publish(out,...)
}


