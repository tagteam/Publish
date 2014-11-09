##' Publishing a matrix in raw, org, latex, or muse format
##' 
##' This is the heart of the Publish package 
##' @param x Matrix to be published
##' @param title Title for table, only in wiki and muse format
##' @param hrule
##' @param colnames
##' @param rownames
##' @param col1name
##' @param digits
##' @param sep
##' @param collapse.head
##' @param collapse.row
##' @param endhead
##' @param endrow
##' @param style
##' @param interLines 
##' @param latex If TRUE use latex table format
##' @param wiki If TRUE use wiki table format
##' @param org If TRUE use org table format
##' @param markdown If TRUE use markdown table format
##' @param environment
##' @param latexTableFormat
##' @param latexHline
##' @param latexNoDollar
##' @param ...
##' @examples
##'
##' x <- matrix(1:12,ncol=3) 
##'
##' 
##' @S3method publish matrix
publish.matrix <- function(x,
                           title,
                           level=1,
                           hrule=0,
                           colnames=TRUE,
                           rownames=TRUE,
                           col1name="",
                           digits=4,
                           sep=" ",
                           collapse.head,
                           collapse.row,
                           endhead,
                           endrow,
                           style,
                           interLines,
                           latex=FALSE,
                           wiki=FALSE,
                           org=FALSE,
                           markdown=FALSE,
                           environment=TRUE,
                           latexTableFormat=NA,
                           latexHline=1,
                           latexNoDollar=FALSE,
                           ...){
    if (missing(interLines))
        interLines <- NULL
    rrr <- rownames(x)
    # {{{ force vectors into matrix form
    if (is.null(dim(x))){
        x <- matrix(x,ncol=length(x))
    }
    # }}}
    # {{{ smartControl
    wiki.DefaultArgs <- list("class"="R-table")
    latex.DefaultArgs <- NULL
    org.DefaultArgs <- NULL
    markdown.DefaultArgs <- NULL
    control <- SmartControl(call=  list(...),
                            keys=c("wiki","latex","markdown","org"),
                            defaults=list("wiki"=wiki.DefaultArgs,"latex"=latex.DefaultArgs,"markdown"=markdown.DefaultArgs,"org"=org.DefaultArgs),
                            ignore.case=TRUE,
                            replaceDefaults=FALSE,
                            verbose=TRUE)
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
                   if (is.na(latexHline))
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
    # {{{ round x
    if (!missing(digits)){
        tmpx <- apply(x,2,function(u){
            if (is.numeric(u) | canbe.numeric(u)){
                format(as.numeric(u),digits=digits,nsmall=digits)}
            else{ u
              }
        })
        rownames(tmpx) <- rownames(x)
        x <- tmpx
    }
    ccc <- colnames(x)
    if (!latex){
        x <- rbind(ccc,x)
        ## x <- format(x,justify="right")
        x <- do.call("cbind",lapply(1:NCOL(x),function(col){
            format(unlist(x[,col]),justify="right")
        }))
        ccc <- x[1,,drop=TRUE]
        x <- x[-1,,drop=FALSE]
    }
    # }}}
    # {{{ colnames & rownames
    if (!is.null(rrr) & rownames){
        if (!is.null(ccc)) ccc <- c(col1name,ccc)
        x <- cbind(Variable=rrr,x)
        x[,1] <- as.character(x[,1])
    }
    # }}}
    # {{{ header
    if (latex && environment==TRUE) {
        if (is.na(latexTableFormat))
            cat("\\begin{tabular}{",c("l|",rep("c",NCOL(x)-1)),"}","\n")
        else
            cat("\\begin{tabular}{",latexTableFormat,"}","\n")
    }
    if (wiki){
        cat("{|","class=\"",control$wiki$class,"\"\n")
        if (!missing(title))
            cat("|+",title,"\n")
    }
    # }}}
    # {{{ insert colunm names
    if (!is.null(interLines[[as.character(0)]]))
        cat(interLines[[as.character(0)]],"\n")
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
    colnames(x) <- NULL
    rownames(x) <- NULL
    # }}}
    # {{{ Cat by row
    if (is.null(dim(x))){
        if (latex && latexNoDollar==FALSE){
            x[grep("<|>|[0-9.]+",x)] <- paste("$",x[grep("<|>|[0-9.]+",x)],"$")
        }
        cat(startrow,paste(x,collapse=collapse.row),endrow)
    }
    else{
        for (r in 1:NROW(x)){
            ## apply(x,1,function(x){
            row.x <- x[r,,drop=TRUE]
            ## extra lines
            if (!is.null(interLines[[as.character(r)]]))
                cat(interLines[[as.character(r)]],"\n")
            ## protect numbers
            if (latex && latexNoDollar==FALSE){#      if (latex)
                row.x[grep("<|>|[0-9.]+",row.x)]=paste("$",row.x[grep("<|>|[0-9.]+",row.x)],"$")
            }
            if (latex && latexHline && x[[1]]!="") cat("\\hline\n")
            cat(startrow,paste(row.x,collapse=collapse.row),endrow)
        }
    }
    # }}}
    # {{{ footer
    if(latex && environment==FALSE)
        NULL
    else
        cat(endtable)
    # }}}
    invisible(x)
}
