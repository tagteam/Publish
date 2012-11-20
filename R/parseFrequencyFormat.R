parseFrequencyFormat <- function(format,digits){
  tmp <- strsplit(format,"[ \t]+|\\(|\\{|\\[|\\)",perl=TRUE)[[1]]
  stats <- tmp[grep("^x$",tmp)-1]
  for(s in 1:length(stats)){
    subs <- switch(stats[s],
                   "count"="%d",
                   "percent"=paste("%1.",digits,"f",sep=""),
                   "colpercent"=paste("%1.",digits,"f",sep=""),
                   stop("Can only count and compute percentages for discrete variables"))
    format <- gsub(paste(stats[s],"(x)",sep=""),subs,format,fixed=TRUE)
  }
  list(format=format,stats=stats)
}
