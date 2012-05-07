baseTable <- function(formula,
                      data,
                      asFactor,
                      asNumeric,
                      sd=TRUE,
                      freq=TRUE,
                      type,
                      order=TRUE,
                      method=1,
                      treatNumericAsFactor=FALSE,
                      dignum=2,
                      printMissing,
                      removeMissingUnits=TRUE){
  m <- model.frame(formula,data,na.action=na.pass)
  if (missing(asNumeric)) asNumeric <- ""
  if (missing(asFactor)) asFactor <- ""
  if (class(asFactor)=="formula") asFactor <- all.vars(asFactor)
  if (class(asNumeric)=="formula") asNumeric <- all.vars(asNumeric)
  names <- names(m)
  NF <- length(names)
  if (missing(type)) type <- sapply(m,function(x){is.factor(x)+2*is.numeric(x)})
  #  type 0=character
  #       1=factor
  #       2=numeric
  if (order) oo <- order(type) else oo <- 1:NF
  names <- names[oo]
  type <- type[oo]
  names(type) <- names
  if (any(match(asFactor,names,nomatch=0))) type[asFactor] <- -1
  if (any(match(asNumeric,names,nomatch=0))) type[asNumeric] <- 2
  
  outColnames <- c("Factor","Levels/Units","Count(Freq)/Mean(SD)","Missing")
  if (all(type==2)) outColnames <- c("Factor","Units","Mean(SD)","Missing")
  if (all(type<2)) outColnames <- c("Factor","Levels","Count(Freq)","Missing")
  out <- lapply(names,function(V){
    typeVar <- type[V]
    valueVar <- m[[V]]
    N <- length(valueVar)
    nonValueVar <- is.na(valueVar)
    miss <- sum(nonValueVar)
    NF <- length(valueVar) - miss
    NU <- length(unique(valueVar))

    if (typeVar<2){
      
      # ------------------Factors and character variables------------------
      
      if (typeVar<=0) valueVar <- factor(valueVar)
      L <- levels(valueVar)
      NL <- length(L)
      tF <- table(valueVar)
      if (freq) tF <- paste(tF," (",round(100*tF/NF,2),"%)",sep="")
      else tF <- as.numeric(as.character(tF))
      outV <- data.frame(V=c(V,rep("",NL-1)),
                         L=L,
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
      if (sd) M <- paste(round(mean(aValue),dignum),
                         " (",
                         ifelse(length(aValue)==1,"--",round(sd(aValue),dignum)),
                         ")",
                         sep="")
      else M <- round(mean(aValue),dignum)
      outV <- data.frame(V,
                         U=units(valueVar),
                         M=M,
                         miss=paste(miss," (",round(100*miss/N,2),"%)",sep=""),
                         stringsAsFactors=FALSE)
      names(outV) <- outColnames
      outV
    }
  })
  if (method==1) {
    out <- do.call("rbind",out)
    if (missing(printMissing)) printMissing <- sum(as.numeric(substr(out[,"Missing"],0,1)>0))
    printMissing <- sum(as.numeric(substr(out[,"Missing"],0,1)>0))
    if (!printMissing) out <- out[,-4,drop=FALSE]
    if (removeMissingUnits) if (all(as.character(out[,2])=="")) out <- out[,-2]
    out
  }
  else{out}
}

