##' Publishing a matrix in raw, org, latex, or muse format
##' 
##' This is the heart of the Publish package
##' @param object Matrix to be published
##' @param title Title for table, only in wiki and muse format
##' @param colnames If \code{TRUE} show column names
##' @param rownames If \code{TRUE} show row names
##' @param col1name Name for first column
##' @param digits Numbers are rounded according to digits
##' @param try.convert Logical. If \code{TRUE} try to convert also non-numeric
##' formats such as character to numeric before rounding. Default is \code{TRUE}.
##' @param sep Field separator when style is \code{"none"}
##' @param endhead String to be pasted at the end of the first row
##'     (header)
##' @param endrow String to be pasted at the end of each row
##' @param style Table style for export to \code{"latex"},
##'     \code{"org"}, \code{"markdown"}, \code{"wiki"},
##'     \code{"none"}. Overwritten by argments below.
##' @param inter.lines A named list which contains strings to be
##'     placed between the rows of the table. An element with name
##'     \code{"0"} is used to place a line before the first column,
##'     elements with name \code{"r"} are placed between line r and
##'     r+1.
##' @param latex If \code{TRUE} use latex table format
##' @param wiki If \code{TRUE} use mediawiki table format
##' @param org If \code{TRUE} use emacs orgmode table format
##' @param markdown If \code{TRUE} use markdown table format
##' @param tabular For style \code{latex} only: if \code{TRUE} enclose
##'     the table in begin/end tabular environement.
##' @param latex.table.format For style \code{latex} only: format of
##'     the tabular environement.
##' @param latex.hline For style \code{latex} only: if \code{TRUE} add
##'     hline statements add the end of each line.
##' @param latex.nodollar For style \code{latex} only: if \code{TRUE}
##'     do not enclose numbers in dollars.
##' @param ... Used to transport arguments. Currently supports
##'     \code{wiki.class}.
##' @examples
##'
##' x <- matrix(1:12,ncol=3)
##' publish(x)
##'
##' # rounding the numeric part of data mixtures 
##' y <- cbind(matrix(letters[1:12],ncol=3),x,matrix(rnorm(12),ncol=3))
##' publish(y,digits=1)
##' 
##' publish(x,inter.lines=list("1"="text between line 1 and line 2",
##'                           "3"="text between line 3 and line 4"))
##' 
##' @export
publish.matrix <- function(object,
                           title,
                           colnames=TRUE,
                           rownames=TRUE,
                           col1name="",
                           digits=4,
                           try.convert=TRUE,
                           sep=" ",
                           endhead,
                           endrow,
                           style,
                           inter.lines,
                           latex=FALSE,
                           wiki=FALSE,
                           org=FALSE,
                           markdown=FALSE,
                           tabular=TRUE,
                           latex.table.format=NA,
                           latex.hline=1,
                           latex.nodollar=FALSE,
                           ...){
    if (is.data.table(object))
        object <- as.matrix(object)
    if (missing(inter.lines))
        inter.lines <- NULL
    rrr <- rownames(object)
    # {{{ force vectors into matrix form
    if (is.null(dim(object))){
        object <- matrix(object,ncol=length(object))
    }
    # }}}
    # {{{ smartControl
    wiki.DefaultArgs <- list("class"="R-table")
    latex.DefaultArgs <- NULL
    org.DefaultArgs <- NULL
    markdown.DefaultArgs <- NULL
    control <- prodlim::SmartControl(call=  list(...),
                            keys=c("wiki","latex","markdown","org"),
                            defaults=list("wiki"=wiki.DefaultArgs,"latex"=latex.DefaultArgs,"markdown"=markdown.DefaultArgs,"org"=org.DefaultArgs),
                            ignore.case=TRUE,
                            replaceDefaults=FALSE,
                            verbose=FALSE)
    # }}}
    # {{{ style dependent syntax
    if (missing(style)) style <- "none"
    if (wiki==TRUE) style <- "wiki"
    if (latex==TRUE) style <- "latex"
    if (org==TRUE) style <- "org"
    if (markdown==TRUE) style <- "markdown"
    switch(style,
           "latex"={
               latex <- TRUE
               wiki <- FALSE
               markdown <- FALSE
               org <- FALSE
               starthead <- ""
               collapse.head <- "&"
               if (missing(endhead)){
                   if (is.na(latex.hline))
                       endhead <- "\\\\\n"
                   else
                       endhead <- "\\\\\\hline\n"
               }
               startrow <- ""
               collapse.row <- "&"
               if (missing(endrow))
                   endrow <- "\\\\\n"
               endtable <- "\\end{tabular}\n"
           },
           "wiki"={
               wiki <- TRUE
               latex <- FALSE
               markdown <- FALSE
               org <- FALSE
               starthead <- "|-\n! "
               collapse.head <- " !! "
               if (missing(endhead)){
                   endhead <- "\n"}
               startrow <- "|-\n| "
               collapse.row <- " || "
               if (missing(endrow))
                   endrow <- "\n"
               endtable <- "|}\n"
           },
           "markdown"={
               wiki <- FALSE
               latex <- FALSE
               markdown <- TRUE
               org <- FALSE
               starthead <- "|"
               collapse.head <- "|"
               if (missing(endhead)){
                   endhead <- "|"
               }
               startrow <- "|"
               collapse.row <- "|"
               if (missing(endrow))
                   endrow <- "|\n"
               endtable <- "\n"
           },
           "org"={
               wiki <- FALSE
               latex <- FALSE
               markdown <- FALSE
               org <- TRUE
               starthead <- "| "
               collapse.head <- " | "
               if (missing(endhead)){
                   endhead <- "|"
               }
               startrow <- "| "
               collapse.row <- " | "
               if (missing(endrow))
                   endrow <- "|\n"
               endtable <- "\n"
           },
           "none"={
               wiki <- FALSE
               latex <- FALSE
               markdown <- FALSE
               org <- FALSE
               starthead <- ""
               collapse.head <- sep
               if (missing(endhead)){
                   endhead <- "\n"
               }
               startrow <- ""
               collapse.row <- sep
               endrow <- "\n"
               endtable <- ""
           })
    # }}}
    # {{{ round object
    if (!missing(digits)){
        tmpx <- apply(object,2,function(u){
            if (is.numeric(u) | (try.convert==TRUE && canbe.numeric(u))){
                format(as.numeric(u),digits=digits,nsmall=digits)}
            else{ u
              }
        })
        if (NROW(object)==1) tmpx <- matrix(tmpx,nrow=1)
        rownames(tmpx) <- rownames(object)
        colnames(tmpx) <- colnames(object)
        object <- tmpx
    }
    if (!is.null(colnames(object)))
        ccc <- matrix(colnames(object),nrow=1)
    else 
        ccc <- rep("",NCOL(object))
    if (!latex){
        object <- rbind(ccc,object)
        ## object <- format(object,justify="right")
        object <- do.call("cbind",lapply(1:NCOL(object),function(col){
            format(unlist(object[,col]),justify="right")
        }))
        ccc <- object[1,,drop=TRUE]
        object <- object[-1,,drop=FALSE]
    }
    # }}}
    # {{{ colnames & rownames
    if (!is.null(rrr) & any(rrr!="") & rownames){
        if (!is.null(ccc)) ccc <- c(col1name,ccc)
        object <- cbind(Variable=rrr,object)
        object[,1] <- as.character(object[,1])
    }
    # }}}
    # {{{ header
    if (latex && tabular==TRUE) {
        if (is.na(latex.table.format))
            cat("\\begin{tabular}{",c("l|",rep("c",NCOL(object)-1)),"}","\n")
        else
            cat("\\begin{tabular}{",latex.table.format,"}","\n")
    }
    if (wiki){
        cat("{|","class=\"",control$wiki$class,"\"\n")
        if (!missing(title))
            cat("|+",title,"\n")
    }
    # }}}
    # {{{ insert colunm names
    if (!is.null(inter.lines[[as.character(0)]]))
        cat(inter.lines[[as.character(0)]],"\n")
    if (!is.null(ccc) && colnames==TRUE){
        cat(starthead,paste(ccc,collapse=collapse.head),endhead)
        if (org==TRUE){
            cat("\n|")
            for (c in 1:length(ccc)){
                if (c==1)
                    cat(paste(rep("-",nchar(ccc[c])+1+nchar(startrow)),collapse=""),sep="")
                else 
                    cat("+",paste(rep("-",nchar(ccc[c]) -1 + nchar(collapse.row)),collapse=""),sep="")
            }
            cat("|\n")
        }
        if (markdown==TRUE){
            cat("\n|")
            for (c in 1:length(ccc)){
                if (c==1)
                    cat(paste(rep("-",nchar(ccc[c]) -1 + nchar(startrow)),collapse=""),sep="")
                else 
                    cat(":|",paste(rep("-",nchar(ccc[c]) -1 + nchar(collapse.row)),collapse=""),sep="")
            }
            cat(":|\n")
        }
    }
    colnames(object) <- NULL
    rownames(object) <- NULL
    # }}}
    # {{{ Cat by row
    if (is.null(dim(object))){
        if (latex && latex.nodollar==FALSE){
            object[grep("<|>|[0-9.]+",object)] <- paste("$",object[grep("<|>|[0-9.]+",object)],"$")
        }
        cat(startrow,paste(object,collapse=collapse.row),endrow)
    }
    else{
        for (r in 1:NROW(object)){
            ## apply(object,1,function(object){
            row.x <- object[r,,drop=TRUE]
            ## extra lines
            if (!is.null(inter.lines[[as.character(r)]]))
                cat(inter.lines[[as.character(r)]],"\n")
            ## protect numbers
            if (latex && latex.nodollar==FALSE){#      if (latex)
                row.x[grep("<|>|[0-9.]+",row.x)]=paste("$",row.x[grep("<|>|[0-9.]+",row.x)],"$")
            }
            if (latex && latex.hline && object[[1]]!="") cat("\\hline\n")
            cat(startrow,paste(row.x,collapse=collapse.row),endrow)
        }
    }
    # }}}
    # {{{ footer
    if(latex && tabular==FALSE)
        NULL
    else
        cat(endtable)
    # }}}
    invisible(object)
}
