##' This function eases the process of generating factor variables
##' with relevant labels. All variables in a data.frame with less than
##' a user set number of levels result in a line which suggests levels and
##' labels. The result can then be modified for use.
##'
##' The code needs to be copy-and-pasted from the R-output
##' buffer into the R-code buffer. This can be customized
##' for the really efficiently working people e.g. in emacs.
##' @title Efficient coding of factor levels
##' @param data Data frame in which to search for categorical variables.
##' @param maxlevels Treat non-factor variables only if the number of unique values less than maxlevels. Defaults to 10.
##' @return R-code one line for each variable.
##' @author Thomas Alexander Gerds
##' @examples
##' data(Diabetes)
##' lazyFactorCoding(Diabetes)
##'
##' @export
lazyFactorCoding <- function(data,maxlevels=10){
    if (!is.character(data))
        data <- as.character(substitute(data))
    d <- get(data)
    isdt <- match("data.table",class(d),nomatch=FALSE)
    out <- lapply(names(d),function(x){
        dx <- d[[x]]
        if ((is.factor(dx) && length(unique(dx))<maxlevels) || (length(unique(dx))<maxlevels)){
            levs.x <- if (is.factor(unique(dx))) levels(dx) else sort(unique(dx))
            labels.x <- paste("\"",paste(levs.x,collapse="\",\"",sep=""),"\"",sep="")
            if (isdt){
                paste0(data,"[",",",x,":=factor(",x,",levels=c(",as.character(labels.x),"),labels=c(",as.character(labels.x),"))]\n")
            }else{
                obj.x <- paste(data,"$",x,sep="")
                paste(obj.x," <- factor(",obj.x,",levels=c(",as.character(labels.x),"),labels=c(",as.character(labels.x),"))\n",sep="")
            }
        }
        else NULL
    })
    out <- out[!sapply(out,is.null)]
    sapply(unlist(out),cat)
    invisible(out)
}
