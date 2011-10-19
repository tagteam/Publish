getType <- function(formula,asNumeric,asFactor,order, data){
  m <- model.frame(formula,data,na.action=na.pass)
  if (missing(asNumeric)) asNumeric <- ""
  if (missing(asFactor)) asFactor <- ""
  if (class(asFactor)=="formula") asFactor <- all.vars(asFactor)
  if (class(asNumeric)=="formula") asNumeric <- all.vars(asNumeric)
  names <- names(m)
  NF <- length(names)
  type <- sapply(m,function(x){is.factor(x)+2*is.numeric(x)})
  #  type 0=character
  #       1=factor
  #       2=numeric
  if (!missing(order) && order) oo <- order(type) else oo <- 1:NF
  names <- names[oo]
  type <- type[oo]
  type <- as.character(factor(type,levels=c(0,1,2),labels=c("character","factor","numeric")))
  names(type) <- names
  if (any(match(asFactor,names,nomatch=0))) type[asFactor] <- "factor"
  if (any(match(asNumeric,names,nomatch=0))) type[asNumeric] <- "numeric"
  type
}
