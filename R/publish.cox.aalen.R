##' @method publish cox.aalen
publish.cox.aalen <- function(object,
                              conf.int = 0.95,
                              robust = FALSE,
                              scale=1,
                              digits = 2,
                              pvalue.digits=4,
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
  pValue <- sapply(pVal,format.pval,digits=pvalue.digits,eps=eps)
  if (mark.pval){
    pValue <- sapply(1: length(pVal), function(p){
      if (pVal[p] < 1-conf.int) paste("**",pValue[p],"**",sep="") 
      else pValue[p]
    })    
  }
  out <- data.frame("Var"=covNames,
               "HR"=format(exp(beta),digits=digits,nsmall=digits),
               "Std.err"=format(format(se,digits=digits,nsmall=digits)),
               "CI.95"=paste("[",format(exp(beta - z * se),digits=digits,nsmall=digits),";",format(exp(beta + z * se),digits=digits,nsmall=digits),"]",sep=""),
               "p-value"=pValue)
  rownames(out) <- rep("",NROW(out))
  if (print==TRUE)
    publish(out,...)
}


