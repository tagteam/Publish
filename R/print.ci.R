print.ci <- function(ci,digits=3,style=2,...){
  if (style==1){
    pci <- cbind(round(cbind(ci[[1]],ci[[2]],ci[[3]],ci[[4]]),digits))
    colnames(pci) <- c(names(ci)[1:2],paste(c("lower","upper"),as.character(100*(1-ci$level)),"%",sep=""))
  }
  else{
    pci <- cbind(round(cbind(ci[[1]],ci[[2]]),digits),
                 apply(cbind(ci[["lower"]],ci[["upper"]]),1,function(x)format.ci(lower=x[1],upper=x[2],style=style,digits=digits)))
    colnames(pci) <- c(names(ci)[1:2],paste("CI-",as.character(100*(1-ci$level)),"%",sep=""))
  }
  pci <- cbind(ci$labels,pci)
  rownames(pci) <- rep("",nrow(pci))
  print(pci,right=FALSE,digits=digits,quote=F)
  invisible(pci)
}
