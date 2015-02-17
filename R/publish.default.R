##' @export
publish.default <- function(object,digits=4,title,bold=TRUE,level=0,hrule=FALSE,title.level,title.hrule,...){
    if (missing(title.level)) title.level <- max(level-1,1)
    if (missing(title.hrule)) title.hrule <- 0
    if (!missing(title)) publish(x=title,level=title.level,hrule=title.hrule)
    if (is.numeric(object) | canbe.numeric(object)){
        x <- format(object,digits=digits,nsmall=digits)
    }
    cat(paste("\n",paste(rep("*",level),collapse=""),ifelse(level>0," ",""),object,"\n",sep=""))
    if (hrule==TRUE) cat("\n----\n")
}
