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
                           muse=FALSE,
                           org=FALSE,
                           environment=TRUE,
                           latexTableFormat=NA,
                           latexHline=1,
                           latexNoDollar=FALSE,
                           ...){
    if (missing(interLines))
        interLines <- NULL
    ccc <- colnames(x)
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
    muse.DefaultArgs <- NULL
    control <- SmartControl(call=  list(...),
                            keys=c("wiki","latex","muse","org"),
                            defaults=list("wiki"=wiki.DefaultArgs,"latex"=latex.DefaultArgs,"muse"=muse.DefaultArgs,"org"=org.DefaultArgs),
                            ignore.case=TRUE,
                            replaceDefaults=FALSE,
                            verbose=TRUE)
    # }}}
    # {{{ style dependent syntax
    if (missing(style)) style <- "none"
    if (wiki==TRUE) style <- "wiki"
    if (latex==TRUE) style <- "latex"
    if (org==TRUE) style <- "org"
    if (muse==TRUE) style <- "muse"
    switch(style,
           "latex"={
               latex <- TRUE
               wiki <- FALSE
               muse <- FALSE
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
               muse <- FALSE
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
           "muse"={
               wiki <- FALSE
               latex <- FALSE
               muse <- TRUE
               org <- FALSE
               starthead <- " "
               collapse.head <- " || "
               if (missing(endhead)){
                   endhead <- "\n"}
               startrow <- " "
               collapse.row <- " | "
               if (missing(endrow))
                   endrow <- "\n"
               endtable <- "\n"
           },
           "org"={
               wiki <- FALSE
               latex <- FALSE
               muse <- FALSE
               org <- TRUE
               starthead <- "| "
               collapse.head <- " | "
               if (missing(endhead)){
                   endhead <- "\n|-----------------\n"
               }
               startrow <- "| "
               collapse.row <- " | "
               if (missing(endrow))
                   endrow <- "\n"
               endtable <- "\n"
           },
           "none"={
               wiki <- FALSE
               latex <- FALSE
               muse <- FALSE
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

  # }}}
  # {{{ colnames & rownames
  if (!is.null(rrr) & rownames){
    if (!is.null(ccc)) ccc <- c(col1name,ccc)
    x <- cbind(Factor=rrr,x)
    x[,1] <- as.character(x[,1])
  }
  # }}}
  # {{{ header
  if (muse && !missing(title)){
    publish(title,level=level,hrule=hrule)
    cat("\n")
  }
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
  # {{{ colunm names
  if (!is.null(interLines[[as.character(0)]]))
        cat(interLines[[as.character(0)]],"\n")
  if (!is.null(ccc) && colnames==TRUE)
    cat(starthead,paste(ccc,collapse=collapse.head),endhead)
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
