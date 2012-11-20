summary.univariateTable <- function(x,order,...){
  if (is.null(x$groups)){
    XX <- all.vars(x$formula)
  }
  else{
    XX <- all.vars(x$formula)[-1]
  }
  order <- match(XX,names(x$summary.groups))
  ordered.summary <- x$summary.groups[order]
  ## totals, missing values (if any) 
  ## factornames, levels
  XXtab <- NULL
  for (s in names(ordered.summary)){
    sum <- as.matrix(ordered.summary[[s]])
    if (!is.null(x$groups)){
      sum <- cbind(sum,Totals=x$summary.totals[[s]])
    }
    else{
      sum <- data.frame(Totals=x$summary.totals[[s]],stringsAsFactors = FALSE)
    }
    if (any(x$missing$totals[[s]]>0)){
      if (is.null(x$groups)){
        miss <- x$missing$totals[[s]]
      }
      else{
        miss <- c(unlist(x$missing$group[[s]]),x$missing$totals[[s]])
      }
    }
    else{
      miss <- NULL
    }
    sum <- rbind(sum,miss)
    if (x$vartype[[s]]=="factor"){
      lev <- x$xlevels[[s]]
    }
    else{
      lev <- paste(x$statistics,collapse="/")
    }
    if (!is.null(miss)) lev <- c(lev,"missing")
    if (!is.null(x$groups)){
      p <- x$p.values[[s]]
      if (NROW(sum)>2 && NROW(p)==(NROW(sum)-1)){
        sum <- cbind(sum,rbind(rep("",NROW(sum)-1),p=x$p.values[[s]]))
        colnames(sum)[NCOL(sum)] <- "p"
      }
      else{
        if (is.null(miss)){
          p <- c(rep("",NROW(sum)-1),x$p.values[[s]])
        }
        else{
          p <- c(rep("",NROW(sum)-2),x$p.values[[s]],"")
        }
        sum <- cbind(sum,p)
      }
    }
    fac <- c(s,rep("",NROW(sum)-1))
    sum <- cbind(unlist(fac),lev,sum)
    ## if (NROW(sum)>2)
    sumXX <- data.frame(sum,stringsAsFactors=FALSE)
    rownames(sumXX) <- NULL
    XXtab <- rbind(XXtab,sumXX)
  }
  rownames(XXtab) <- 1:NROW(XXtab)
  if (is.null(x$groups)){
    colnames(XXtab) <- c("Factor","Levels",x$groups,"Count/Summary")
  }
  else{
    colnames(XXtab) <- c("Factor","Levels",x$groups,"Totals","Pvalues")
  }
  rownames(XXtab) <- NULL
  ## print(XXtab,quote=FALSE)
  XXtab
}
