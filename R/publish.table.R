publish.table <- function(x,title,level,...){
  if ((NM=length(dim(x)))>2){
    if (missing(title)) title <- ""
    stopifnot(NM<=4)
    invisibleOut=lapply(1:(dim(x)[NM]),function(m){
      if (NM==3){
        newtitle=paste(title,"\n",paste(names(dimnames(x))[NM],dimnames(x)[[NM]][m],sep="="))
        publish(x[,,m],title=newtitle)
      }
      else{
        newtitle=paste(title,"\n",paste(names(dimnames(x))[NM],dimnames(x)[[NM]][m],sep="="))
        publish(x[,,,m],title=newtitle)
      }
    })
  }
  else{
    v <- as.matrix(x)
    nn <- names(dimnames(v))
    if (is.null(nn))
      if (is.matrix(x)) nn <- paste("Var",1:2,sep=".")
      else nn <- "Var.1"
    nn[nn==""] <- paste("Var",(1:length(nn))[nn==""],sep=".")
    rownames <- TRUE
    ## if (missing(title)) title <- paste("Frequency table:",nn[1],"versus",nn[2],sep=" ")
    if (missing(title)) title <- ""
    if (missing(level)) level <- 0
    publish.matrix(v,title,level=level,rownames=rownames,...)
  }
}
