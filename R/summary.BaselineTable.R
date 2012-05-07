summary.BaselineTable <- function(x,order,...){
  XX <- all.vars(x$formula)[-1]
  order <- match(XX,names(x$summary.groups))
  ordered.summary <- x$summary.groups[order]
  ## totals, missing values (if any) 
  ## factornames, levels
  XXtab <- NULL
  for (s in names(ordered.summary)){
    sum <- ordered.summary[[s]]
    sum <- cbind(sum,Totals=x$summary.totals[[s]])
    if (any(x$missing$totals[[s]]>0)){
      miss <- c(x$missing$totals[[s]],unlist(x$missing$group[[s]]))
    }
    else{
      miss <- NULL
    }
    sum <- rbind(sum,miss)
    if (x$vartype[[s]]==1){
      lev <- x$xlevels[[s]]
    }
    else{
      lev <- paste(x$statistics,collapse="/")
    }
    if (!is.null(miss)) lev <- c(lev,"missing")
    if (is.null(miss)){
      p <- c(rep("",NROW(sum)-1),x$p.values[[s]])
    }
    else{
      p <- c(rep("",NROW(sum)-2),x$p.values[[s]],"")
    }
    sum <- cbind(sum,p)
    fac <- c(s,rep("",NROW(sum)-1))
    sum <- cbind(fac,lev,sum)
    XXtab <- rbind(XXtab,data.frame(sum,stringsAsFactors=FALSE))
  }
  XXtab <- data.frame(XXtab,row.names=1:NROW(XXtab))
  colnames(XXtab) <- c("Factor","Levels",x$groups,"Totals","Pvalues")
  ## print(XXtab,quote=FALSE)
  XXtab
}
