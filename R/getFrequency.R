getFrequency <- function(matrix,varnames,groupvar,groups,labels,stats,format){
  totals <- vector(NCOL(matrix),mode="list")
  names(totals) <- varnames
  groupfreq <- vector(NCOL(matrix),mode="list")
  names(groupfreq) <- varnames
  for (v in varnames){
    ## vv <- matrix[,grep(paste(v,":",sep=""),colnames(matrix))]
    vv <- matrix[,v]
    missing.v <- is.na(vv)
    vvv <- as.factor(vv[!missing.v])
    ggg <- factor(groupvar[!missing.v],levels=levels(groupvar))
    levels.x <- levels(vvv)
    ## totals
    if (is.null(groupvar)){
      tab.v <- table(vvv) 
      perc.v <- 100*tab.v/sum(tab.v)
      totals[[v]] <- sapply(1:length(perc.v),function(i){
        values <- list(tab.v[i],perc.v[i])
        names(values) <- c("count","percent")
        do.call("sprintf",c(format,values[stats]))
      })
    } 
    else{
      totals[[v]] <- table(vvv)
    }
    ## groups
    if (!is.null(groupvar) && !missing(groupvar) && length(groupvar)==NROW(matrix)){
      tables <- lapply(split(ggg,vvv),function(x){
        xtab <- data.frame(table(factor(x,levels=groups)))
        if (match("percent",stats,nomatch=FALSE)){
          xtab$Percent <- 100*xtab$Freq/sum(xtab$Freq)
        }
        tab.out <- lapply(1:NROW(xtab),function(row){
          values <- xtab[row,-1]
          if (match("colpercent",stats,nomatch=FALSE)){
            values
          }
          else{
            vals <- as.list(unlist(values))
            if (match("count",stats,nomatch=FALSE)){
              do.call("sprintf",c(format,vals))
            }else{ ## only percent
              do.call("sprintf",c(format,vals["Percent"]))
            }
          }
        })
        names(tab.out) <- labels
        unlist(tab.out)
      })
      groupfreq[[v]] <- do.call("rbind",tables)
      if (match("colpercent",stats,nomatch=FALSE)){
        groupfreq[[v]] <- apply(groupfreq[[v]],2,function(x){
          val <- as.numeric(x)
          colp <- 100*val/sum(val)
          if (match("count",stats,nomatch=FALSE)){
            sapply(1:length(val),function(i){
              do.call("sprintf",c(format,as.list(c(val[i],colp[i]))))
            })
          }else{ ## only colpercent
            sapply(1:length(val),function(i){
              do.call("sprintf",c(format,as.list(colp[i])))
            })
          }
        })
      }
    }
  }
  list(totals=totals,groupfreq=groupfreq)
}
