publish.matrix <- function(x,
                           title,
                           level=1,
                           hrule=0,
                           colnames=TRUE,
                           rownames=TRUE,
                           col1name="",
                           digits=4,
                           collapse.head,
                           collapse.row,
                           endrow,
                           style="muse",
                           latex=FALSE,
                           wiki=FALSE,
                           environment=TRUE,
                           ...){
 
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
  muse.DefaultArgs <- NULL
  control <- SmartControl(call=  list(...),
                          keys=c("wiki","latex","muse"),
                          defaults=list("wiki"=wiki.DefaultArgs,"latex"=latex.DefaultArgs,"muse"=muse.DefaultArgs),
                          ignore.case=TRUE,
                          replaceDefaults=FALSE,
                          verbose=TRUE)
  # }}}
  # {{{ style dependent syntax
  if (wiki==TRUE) style <- "wiki"
  if (latex==TRUE) style <- "latex"
  switch(style,
         "latex"={
           latex <- TRUE
           wiki <- FALSE
           muse <- FALSE
           starthead <- ""
           collapse.head <- "&"
           endhead <- "\\\\\n"
           startrow <- ""
           collapse.row <- "&"
           endrow <- "\\\\\n"
           endtable <- "\n\\end{tabular}\n"
         },
         "wiki"={
           wiki <- TRUE
           latex <- FALSE
           muse <- FALSE
           starthead <- "|-\n! "
           collapse.head <- " !! "
           endhead <- "\n"
           startrow <- "|-\n| "
           collapse.row <- " || "
           endrow <- "\n"
           endtable <- "|}\n"
         },
         "muse"={
           wiki <- FALSE
           latex <- FALSE
           muse <- TRUE
           starthead <- " "
           collapse.head <- " || "
           endhead <- "\n"
           startrow <- " "
           collapse.row <- " | "
           endrow <- "\n"
           endtable <- "\n"
         })
  # }}}
  # {{{ round x
  if (!missing(digits)){
    tmpx <- apply(x,2,function(u){
      if (is.numeric(u) | canbe.numeric(u)){
        round(as.numeric(u),digits=digits)}
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
    x <- cbind(rrr,x)
  }
  # }}}
  # {{{ header
  if (muse && !missing(title)){
    publish(title,level=level,hrule=hrule)
    cat("\n")
  }
  if (latex && environment==TRUE) {
    cat("\\begin{tabular}{",rep("l",NCOL(x)),"}","\n")
  }
  if (wiki){
    cat("{|","class=\"",control$wiki$class,"\"\n")
    if (!missing(title))
      cat("|+",title,"\n")
  }
  # }}}
  # {{{ colunm names
  if (!is.null(ccc) && colnames==TRUE)
    cat(starthead,paste(ccc,collapse=collapse.head),endhead)
  colnames(x) <- NULL
  rownames(x) <- NULL
  # }}}
  # {{{ Cat by row
  if (is.null(dim(x))){
    if (latex) x[grep("<|>|[0-9.]+",x)]=paste("$",x[grep("<|>|[0-9.]+",x)],"$")
    cat(startrow,paste(x,collapse=collapse.row),endrow)
  }
  else{
  apply(x,1,function(x){
    ## protect numbers
    if (latex) x[grep("<|>|[0-9.]+",x)]=paste("$",x[grep("<|>|[0-9.]+",x)],"$")
    cat(startrow,paste(x,collapse=collapse.row),endrow)
  })}
  # }}}
  # {{{ footer
  if(latex && environment==FALSE)
    NULL
  else
    cat(endtable)
  # }}}
  invisible(x)
}
