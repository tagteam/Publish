##' For all discrete and factor variables in a data set write R-code
##' which can quickly be modified to change the levels.
##' 
##' The code needs to be copy-and-pasted from the R-output
##' buffer into the R-code buffer. This can be customized
##' for the really efficiently working people e.g. in emacs.
##' @title Efficient coding of factor levels
##' @param data
##' @param maxlevel Treat non-factor variables only if the number of unique values less than maxlevel. Defaults to 10.
##' @return R-code one line for each variable.
##' @author Thomas Alexander Gerds
##' @export
lazyFactorCoding <- function(data,maxlevel=10){
    if (!is.character(data))
        data <- as.character(substitute(data))
    d <- get(data)
    out <- lapply(names(d),function(x){
        if ((is.factor(d[,x]) && length(unique(d[,x]))<maxlevel) || length(unique(d[,x]))<maxlevel){
            obj.x <- paste(data,"$",x,sep="")
            levs.x <- if (is.factor(d[,x])) levels(d[,x]) else sort(unique(d[,x]))
            labels.x <- paste("\"",paste(levs.x,collapse="\",\"",sep=""),"\"",sep="")
            ## levs.x <- paste(levs.x,collapse=",",sep="")
            paste(obj.x," <- factor(",obj.x,",levels=c(",as.character(labels.x),"),labels=c(",as.character(labels.x),"))\n",sep="")
        }
        else NULL
    })
    out <- out[!sapply(out,is.null)]
    sapply(unlist(out),cat)
    invisible(out)
}
