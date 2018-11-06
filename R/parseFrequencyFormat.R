parseFrequencyFormat <- function(format,digits){
    tmp <- strsplit(format,"[ \t]+|[^ \t]*=|[^ \t]*:|[^ \t]*-|[^ \t]*\\+|\\(|\\{|\\[|\\)",perl=TRUE)[[1]]
    stats <- tmp[grep("^x$",tmp)-1]
    for(s in 1:length(stats)){
        subs <- switch(stats[s],
                       "count"="%s",
                       "total"="%s",
                       "percent"="%s", #paste("%1.",digits,"f",sep=""),
                       "colpercent"="%s", #paste("%1.",digits,"f",sep=""),
                       stop(paste("Cannot parse function ",
                                  stats[s],
                                  ". ",
                                  "Can only parse count, total and compute percentages for categorical variables",
                                  sep="")))
        format <- gsub(paste(stats[s],"(x)",sep=""),subs,format,fixed=TRUE)
    }
    list(format=format,stats=stats)
}
