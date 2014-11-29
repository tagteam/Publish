parseSummaryFormat <- function(format,digits){
    S <- function(x,format,digits,nsmall){x}
    F <- function(x,ref,digits,nsmall){x}
    iqr <- function(x)quantile(x,c(0.25,0.75))
    minmax <- function(x)quantile(x,c(0,1))
    CI.95 <- function(x,sep=",",...){
        m <- ci.mean.default(x,...)
        paste(format(m$lower,digits=digits,nsmall=digits),
              sep," ",
              format(m$upper,digits=digits,nsmall=digits))
    }
    format.numeric <- paste("%1.",digits,"f",sep="")
    tmp <- strsplit(format,"[ \t]+|\\(|\\{|\\[|\\)",perl=TRUE)[[1]]
    stats <- tmp[grep("^x$",tmp)-1]
    outclass <- sapply(stats,function(s)class(do.call(s,list(1:2))))
    outlen <- sapply(stats,function(s)length(do.call(s,list(1:2))))
    for(s in 1:length(stats)){
        subs <- format.numeric
        if(!(outlen[s]%in%c(1,2)))
            stop(paste("The function",stats[s],"returns",outlen[s],"values (can be 1 or 2)"))
        subs <- switch(as.character(outlen[s]),
                       "1"={switch(outclass[s],
                                   "numeric"=format.numeric,
                                   "integer"=format.numeric,
                                   "%s")},
                       "2"={switch(outclass[s],
                                   "numeric"=paste(format.numeric,", ",format.numeric,sep=""),
                                   "integer"=paste(format.numeric,", ",format.numeric,sep=""),
                                   paste("%s",", ","%s",sep=""))})
        format <- gsub(paste(stats[s],"(x)",sep=""),subs,format,fixed=TRUE)
    }
    list(format=format,stats=stats)
}
