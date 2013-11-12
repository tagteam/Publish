##' @S3method publish default
publish.default <- function(x,digits=4,title,bold=TRUE,level=0,hrule=FALSE,title.level,title.hrule,...){
  if (missing(title.level)) title.level <- max(level-1,1)
  if (missing(title.hrule)) title.hrule <- 0
  if (!missing(title)) publish(x=title,level=title.level,hrule=title.hrule)
  if (is.numeric(x) | canbe.numeric(x)){
      x <- format(x,digits=digits,nsmall=digits)
  }
  cat(paste("\n",paste(rep("*",level),collapse=""),ifelse(level>0," ",""),x,"\n",sep=""))
  if (hrule==TRUE) cat("\n----\n")
}
