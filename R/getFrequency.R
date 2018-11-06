getFrequency <- function(matrix,
                         varnames,
                         groupvar,
                         groups,
                         labels,
                         stats,
                         format,digits,big.mark=","){
    totals <- vector(NCOL(matrix),mode="list")
    xlevels <- vector(NCOL(matrix),mode="list")
    names(totals) <- varnames
    groupfreq <- vector(NCOL(matrix),mode="list")
    names(groupfreq) <- varnames
    for (v in varnames){
        vv <- matrix[,v,drop=FALSE]
        missing.v <- is.na(vv)
        if (is.factor(vv[[1]]))
            vvv <- factor(vv[!missing.v],levels=levels(vv[[1]]))
        else
            vvv <- factor(vv[!missing.v],levels=unique(vv[[1]]))
        xlevels[[v]] <- levels(vvv)
        ggg <- factor(groupvar[!missing.v],
                      levels=levels(groupvar))
        ## totals
        tab.v <- table(vvv)
        total.v <- sum(tab.v)
        s.tab.v <- sum(tab.v)
        if ("colpercent" %in% stats)
            perc.v <- (100*tab.v/s.tab.v)
        else
            perc.v <- rep(100,length(names(tab.v)))
        ## avoid NA when 0/0
        perc.v[s.tab.v==0] <- 0
        # format percent
        perc.v <- lapply(perc.v,function(p){
            sprintf(fmt=paste("%1.",digits,"f",sep=""),p)
        })
        totals[[v]] <- sapply(1:length(perc.v),function(i){
            values <- list(tab.v[i],total.v,perc.v[i])
            if ("colpercent" %in% stats)
                names(values) <- c("count","total","colpercent")
            else
                names(values) <- c("count","total","percent")
            if (big.mark!="") values[["count"]] <- format(values[["count"]],big.mark=big.mark,scientific=FALSE)
            do.call("sprintf",c(format,values[stats]))
        })
        ## 
        ## groups
        ##
        if (!is.null(groupvar) && !missing(groupvar) && length(groupvar)==NROW(matrix)){
            tables <- lapply(split(ggg,vvv),function(x){
                xtab <- data.frame(table(factor(x,levels=groups)))
                if (match("percent",stats,nomatch=FALSE)){
                    xtab$Percent <- 100*xtab$Freq/sum(xtab$Freq)
                    ## avoid NA when 0/0
                    xtab$Percent[xtab$Freq==0] <- 0
                    # format percent
                    xtab$Percent <- sprintf(fmt=paste("%1.",digits,"f",sep=""),xtab$Percent)
                }
                tab.out <- lapply(1:NROW(xtab),function(row){
                    values <- xtab[row,-1]
                    if (match("colpercent",stats,nomatch=FALSE)){
                        values
                    }
                    else{
                        vals <- as.list(unlist(values))
                        if (pos.count <- match("count",stats,nomatch=FALSE)){
                            if (big.mark!="") vals[[pos.count]] <- format(vals[[pos.count]],big.mark=big.mark,scientific=FALSE)
                            if (pos.count==1)
                                do.call("sprintf",c(format,vals))
                            else # pos.count==2
                                do.call("sprintf",c(format,rev(vals)))
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
                    ## avoid NA when 0/0
                    colp[sum(val)==0] <- 0
                    # format percent
                    colp <- sprintf(fmt=paste("%1.",digits,"f",sep=""),colp)
                    if (pos.count <- match("count",stats,nomatch=FALSE)){
                        if (big.mark!="") val[[pos.count]] <- format(val[[pos.count]],big.mark=big.mark,scientific=FALSE)
                        sapply(1:length(val),function(i){
                            if (pos.count==1)
                                do.call("sprintf",c(format,as.list(c(val[i],colp[i]))))
                            else # pos.count==2
                                do.call("sprintf",c(format,as.list(c(colp[i],val[i]))))
                        })
                    }else{ ## show colpercent without count
                        sapply(1:length(val),function(i){
                            do.call("sprintf",c(format,as.list(colp[i])))
                        })
                    }
                })
                ## for "variables" with only one level
                if (length(tables)==1){
                    groupfreq[[v]] <- matrix(groupfreq[[v]],ncol=length(tables[[1]]))
                    colnames(groupfreq[[v]]) <- names(tables[[1]])
                }
            }
        }
    }
    list(totals=totals,groupfreq=groupfreq,xlevels=xlevels)
}
