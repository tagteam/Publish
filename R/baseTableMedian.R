baseTableMedian <- function(formula,
                            asFactor,
                            asNumeric,
                            minmax=FALSE,
                            iqr=TRUE,
                            freq=TRUE,
                            data,
                            order=FALSE,
                            method=1,
                            treatNumericAsFactor=FALSE,
                            dignum=2,
                            printMissing,
                            removeMissingUnits=TRUE,
                            longlevelnames=TRUE){
  m <- model.frame(formula,data,na.action=na.pass)
  names <- names(m)
  NF <- length(names)
  type <- getType(formula=formula,asFactor=asFactor,asNumeric=asNumeric,order=order,data=data)
  outColnames <- c("Factor","Count(Freq)/Median(IQR)","Missing")
  if (all(type=="numeric")) outColnames <- c("Factor","Median(IQR)","Missing")
  if (all(type!="numeric")) outColnames <- c("Factor","Count(Freq)","Missing")
  if (minmax) outColnames <- c(outColnames,"MinMax")
  out <- lapply(names,function(V){
    typeVar <- type[V]
    valueVar <- m[[V]]
    N <- length(valueVar)
    nonValueVar <- is.na(valueVar)
    
    miss <- sum(nonValueVar)
    NF <- length(valueVar) - miss
    NU <- length(unique(valueVar))
    if (typeVar!="numeric"){
      
      # ------------------Factors and character variables------------------
      
      if (typeVar=="character") valueVar <- factor(valueVar)
      L <- levels(valueVar)
      tF <- table(valueVar)
      if (freq) tF <- paste(tF," (",round(100*tF/NF,2),"%)",sep="")
      else tF <- as.numeric(as.character(tF))
      if (longlevelnames==TRUE)
        lln=paste(V,L,sep="=")
      else lln= L
      outV <- data.frame(V=lln,
                         tF=tF,
                         miss=paste(miss," (",round(100*miss/N,2),"%)",sep=""),
                         stringsAsFactors=FALSE)
      names(outV) <- outColnames
      outV
    }
    else{
      
      # -------------------------Numeric variables-------------------------
      
      aValue <- valueVar[!nonValueVar]
      require(rms)

      if (iqr) M <- paste(round(median(aValue),dignum),
                         " (",
                         ifelse(length(aValue)==1,"--",iqr(aValue,dignum)),
                         ")",
                         sep="")
      else M <- round(median(aValue),dignum)

      if (minmax)
        min.max <- paste(" ",round(min(aValue),dignum)," -- ",round(max(aValue),dignum),"",sep="")
      outV <- data.frame(V,M=M,stringsAsFactors=FALSE)
      outV <- cbind(outV,miss=paste(miss," (",round(100*miss/N,2),"%)",sep=""),stringsAsFactors=FALSE)
      if (minmax) outV <- cbind(outV,minmax=as.character(min.max),stringsAsFactors=FALSE)
      names(outV) <- outColnames
      outV
    }
  })
  if (method==1) {
    out <- do.call("rbind",out)
    if (missing(printMissing)) printMissing <- sum(as.numeric(substr(out[,"Missing"],0,1)>0))
    printMissing <- sum(as.numeric(substr(out[,"Missing"],0,1)>0))
    if (!printMissing) out <- out[,-grep("Missing",names(out)),drop=FALSE]
    if (removeMissingUnits) if (all(as.character(out[,2])=="")) out <- out[,-2]
    out
  }
  else{out}
}

