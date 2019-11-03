##' This function eases the process of generating date variables.
##' All variables in a data.frame which match a regular expression
##' are included
##'
##' The code needs to be copy-and-pasted from the R-output
##' buffer into the R-code buffer. This can be customized
##' for the really efficiently working people, e.g., in emacs.
##' @title Efficient coding of date variables
##' @param data Data frame in which to search for date variables.
##' @param format passed to as.Date
##' @param pattern match date variables
##' @param varnames variable names
##' @param testlength how many rows of data should be evaluated to guess the format.
##' @return R-code one line for each variable.
##' @author Thomas Alexander Gerds
##' @examples
##' d <- data.frame(x0="190101",x1=c("12/8/2019"),x2="12-8-2019",x3="20190812",stringsAsFactors=FALSE)
##' lazyDateCoding(d,pattern="x")
##' lazyDateCoding(d,pattern="3")
##'
##' @export
lazyDateCoding <- function(data,format,pattern,varnames,testlength=10){
    if (!is.character(data))
        data <- as.character(substitute(data))
    d <- get(data, envir=parent.frame())
    isdt <- match("data.table",class(d),nomatch=FALSE)
    datevars <- grep(pattern,names(d),value=TRUE)
    out <- lapply(datevars,function(x){
        dx <- d[[x]]
        if (is.character(dx)){
            test.x <- dx[!is.na(dx)]
            test.x <- test.x[1:(min(length(test.x),testlength))]
            ## separator
            separators <- c("-","/","\\|"," ")
            sep <- sapply(separators,grep,test.x,value=TRUE)
            lsep <- sapply(sep,length)
            if (all(lsep==0)) 
                sep <- ""
            else
                sep <- names(sep)[lsep==max(lsep)]
            ## day
            day <- "%d"
            ## month m or b
            if (any(grepl("[:alpha:]",test.x)))
                month <- "%b" else month <- "%m"
            ## year 07 or 2007
            l.x <- nchar(test.x)
            if (any((l.x-2*nchar(sep))<=6))
                year <- "%y" else year <- "%Y"
            ## order
            test.formats <- c(paste0(day,sep,month,sep,year),
                              paste0(day,sep,year,sep,month),
                              paste0(year,sep,month,sep,day),
                              paste0(year,sep,day,sep,month),
                              paste0(month,sep,year,sep,day),
                              paste0(month,sep,day,sep,year))
            if (sep!=""){
                list.x <- strsplit(test.x,sep)
                Y <- match(4,nchar(list.x[[1]]),nomatch=0)
                if (Y>0) year <- "%Y"
                test.formats <- switch(as.character(Y),
                                       "3"={c(paste0(day,sep,month,sep,year),
                                              paste0(month,sep,day,sep,year))},
                                       "1"={c(paste0(year,sep,month,sep,day),
                                              paste0(year,sep,day,sep,month))},
                                       "2"={c(paste0(month,sep,year,sep,day),
                                              paste0(day,sep,year,sep,month))},
                                       {test.formats})
            }
            ## print(test.formats)
            nix <- try(this.x <- as.Date(test.x[[1]],format=test.formats))
            if ((class(nix)[[1]]=="try-error") || all(is.na(this.x))){
                format.x <- "dontknow"
            }else{
                format.x <- test.formats[!is.na(this.x)]
                if (length(format.x)>1){  # multiple matches
                    winner <- sapply(format.x,function(fx){sum(!is.na(as.Date(test.x,format=fx)))})
                    format.x <- format.x[winner==max(winner)][1]
                }
            }
            if (isdt){
                paste0(data,"[",",",x,":=as.Date(",x,",format=\"",format.x,"\")]\n")
            }else{
                obj.x <- paste(data,"$",x,sep="")
                paste(obj.x," <- as.Date(",obj.x,",format=c(\"",format.x,"\")\n",sep="")
            }
        }else{
            NULL
        }})
    out <- out[!sapply(out,is.null)]
    sapply(unlist(out),cat)
    invisible(out)
}
