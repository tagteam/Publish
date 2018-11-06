getSummary <- function(matrix,
                       varnames,
                       groupvar,
                       groups,
                       labels,
                       stats,
                       format,
                       digits,big.mark){
    iqr <- function(x)quantile(x,c(0.25,0.75))
    minmax <- function(x)quantile(x,c(0,1))
    CI.95 <- function(x,sep=";",big.mark=big.mark,...){
        m <- ci.mean.default(x,...)
        paste(format(m$lower,digits=digits,nsmall=digits,bigmark=big.mark),
              sep," ",
              format(m$upper,digits=digits,nsmall=digits,bigmark=big.mark),sep="")
    }
    totals <- vector(NCOL(matrix),mode="list")
    names(totals) <- varnames
    groupsummary <- vector(NCOL(matrix),mode="list")
    names(groupsummary) <- varnames
    for (v in varnames){
        vv <- matrix[,v]
        missing.v <- is.na(vv)
        vvv <- vv[!missing.v]
        totals.values <- lapply(stats,function(s){
            do.call(s,list(vvv))
        })
        specialUnlist <- function(list){
            if (any(sapply(list,function(l){length(l)})>1)){
                ll <- lapply(list,function(x){
                    if (length(x)>1) as.list(x) else x
                })
                return(as.list(unlist(ll,recursive=FALSE)))
            }
            else{
                return(list)
            }
        }
        totals.values <- lapply(totals.values,function(x){
            a <- sprintf(fmt=paste("%1.",digits,"f",sep=""),x)
            if (big.mark!="")
                a <- format(as.numeric(a),big.mark=big.mark,scientific=FALSE)
            a
        })
        totals[[v]] <- do.call("sprintf",c(format,specialUnlist(totals.values)))
        if (!is.null(groupvar) && !missing(groupvar) && length(groupvar)==NROW(matrix)){
            ggg <- factor(groupvar[!missing.v],levels=groups)
            gsum.v <- lapply(groups,function(g){
                values <- lapply(stats,function(s){
                    do.call(s,list(vvv[ggg==g]))
                })
                values <- lapply(values,function(x){
                    a <- sprintf(fmt=paste("%1.",digits,"f",sep=""),x)
                    if (big.mark!="")
                        a <- format(as.numeric(a),big.mark=big.mark,scientific=FALSE)
                    a
                })
                do.call("sprintf",c(format, specialUnlist(values)))
            })
            names(gsum.v) <- labels
            groupsummary[[v]] <- do.call("cbind", gsum.v)
        }
    }
    list(totals=totals,groupsummary=groupsummary)
}
