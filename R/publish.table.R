##' @export
publish.table <- function(object,title,level,...){
  if ((NM=length(dim(object)))==3){
    if (missing(title)) title <- ""
    stopifnot(NM<=4)
    invisibleOut=lapply(1:(dim(object)[NM]),function(m){
      newtitle=paste(title,paste(names(dimnames(object))[NM],dimnames(object)[[NM]][m],sep=":"))
      xm <- object[,,m]
      colnames(xm) <- paste(names(dimnames(object))[2],dimnames(object)[[2]],sep=":")
      rownames(xm) <- paste(names(dimnames(object))[1],dimnames(object)[[1]],sep=":")
      publish(xm,title=newtitle,level=level)
    })
  }
  else{
    v <- as.matrix(object)
    nn <- names(dimnames(v))
    if (is.null(nn))
      if (is.matrix(object)) nn <- paste("Var",1:2,sep=".")
      else nn <- "Var.1"
    nn[nn==""] <- paste("Var",(1:length(nn))[nn==""],sep=".")
    rownames <- TRUE
    ## if (missing(title)) title <- paste("Frequency table:",nn[1],"versus",nn[2],sep=" ")
    if (missing(title)) title <- ""
    if (missing(level)) level <- 0
    publish.matrix(v,title,level=level,rownames=rownames,...)
  }
}
