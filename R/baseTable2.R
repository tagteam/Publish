# {{{ header
baseTable2 <- function(formula,
                       asFactor,
                       asNumeric,
                       sd=TRUE,
                       se=FALSE,
                       minmax=FALSE,
                       median=FALSE,
                       IQR=median,
                       freq=TRUE,
                       data,
                       order=FALSE,
                       method=1,
                       treatNumericAsFactor=FALSE,
                       dignum=2,
                       printMissing=TRUE,
                       removeMissingUnits=TRUE,
                       longlevelnames=TRUE,
                       Totals=TRUE){
  # }}}
  # {{{ specify the data
  m <- model.frame(formula,data,na.action=na.pass)
  names <- names(m)
  NF <- length(names)
  type <- getType(formula=formula,asFactor=asFactor,asNumeric=asNumeric,order=order,data=data)
  outColnames <- c("Factor","Count(Freq)/Mean(SD)")
  if (all(type=="numeric")) outColnames <- c("Factor","Mean(SD)")
  if (all(type!="numeric")) outColnames <- c("Factor","Count(Freq)")
  if (se) outColnames <- c(outColnames,"SE")
  if (median)
    if (IQR)
      outColnames <- c(outColnames,"Median (IQR)")
    else
      outColnames <- c(outColnames,"Median")
  if (minmax) outColnames <- c(outColnames,"MinMax")
  outColnames <- c(outColnames,"Missing")
  out <- lapply(names,function(V){
    typeVar <- type[V]
    valueVar <- m[[V]]
    N <- length(valueVar)
    nonValueVar <- is.na(valueVar)
    miss <- sum(nonValueVar)
    NF <- length(valueVar) - miss
    NU <- length(unique(valueVar))
    # }}}
    # {{{ Factors and character variables
    if (typeVar!="numeric"){
      if (typeVar=="character") valueVar <- factor(valueVar)
      L <- levels(valueVar)
      tF <- table(valueVar)
      if (Totals)
        tF <- c(tF,total=sum(tF))
      if (freq) tF <- paste(tF," (",round(100*tF/NF,2),"%)",sep="")
      else tF <- as.numeric(as.character(tF))
      if (minmax)
        min.max <- paste(" ",L[1]," -- ",L[length(L)],"",sep="")
      if (longlevelnames==TRUE)
        lln=paste(V,L,sep="=")
      else lln= L
      if(Totals){
        V <- c(lln,"Total")
      }else{
        V <- lln
      }
      outV <- data.frame(V=V,
                         tF=tF,
                         stringsAsFactors=FALSE)
      if (median) outV <- cbind(outV,median=L[median(1:length(L))])
      if (minmax) outV <- cbind(outV,minmax=as.character(min.max),stringsAsFactors=FALSE)
      outV$miss <- paste(miss," (",round(100*miss/N,2),"%)",sep="")
      names(outV) <- outColnames
      outV
    }
    # }}}
    # {{{ Numeric variables
    else{
      aValue <- valueVar[!nonValueVar]
      require(rms)
      if (sd) M <- paste(round(mean(aValue),dignum)," (",ifelse(length(aValue)==1,"--",round(sd(aValue),dignum)),")",sep="")
      else M <- round(mean(aValue),dignum)
      if (minmax)
        min.max <- paste(" ",round(min(aValue),dignum)," -- ",round(max(aValue),dignum),"",sep="")
      if (se)
        SE <- round(sd(aValue)/sqrt(N),dignum)
      if (median)
        if (IQR){
          Med <- paste(round(median(aValue),dignum)," (",ifelse(length(aValue)==1,"--",round(quantile(aValue,c(.25)),dignum)),";",ifelse(length(aValue)==1,"",round(quantile(aValue,c(.75)),dignum)),")",sep="")}
        else Med <- round(median(aValue),dignum)
      outV <- data.frame(V,M=M,stringsAsFactors=FALSE)
      if (se) outV <- cbind(outV,SE=as.character(SE),stringsAsFactors=FALSE)
      if (median) outV <- cbind(outV,median=as.character(Med),stringsAsFactors=FALSE)
      if (minmax) outV <- cbind(outV,minmax=as.character(min.max),stringsAsFactors=FALSE)
      outV <- cbind(outV,miss=paste(miss," (",round(100*miss/N,2),"%)",sep=""),stringsAsFactors=FALSE)
      names(outV) <- outColnames
      outV
    }
  })
  # }}}
  # {{{ sample size
  ## samplesizeLine <- as.data.frame(matrix(rep("-",length(outColnames)),nrow=1))
  ## names(samplesizeLine) <- outColnames
  ## samplesizeLine[,1] <- "SampleSize"
  ## samplesizeLine[,2] <- NROW(m)
  ## out <- c(out,list(samplesizeLine))
  # }}}
  # {{{ output format
  if (method==1) {
    out <- do.call("rbind",out)
    ## if (missing(printMissing)) printMissing <- sum(as.numeric(substr(out[,"Missing"],0,1)>0))
    ## printMissing <- sum(as.numeric(substr(out[,"Missing"],0,1)>0))
    if (!printMissing) out <- out[,-grep("Missing",names(out)),drop=FALSE]
    if (removeMissingUnits) if (all(as.character(out[,2])=="")) out <- out[,-2]
    out
  }
  else{out}
  # }}}
}

