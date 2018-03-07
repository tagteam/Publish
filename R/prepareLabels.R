### prepareLabels.R --- 
#----------------------------------------------------------------------
## author: Thomas Alexander Gerds
## created: May 13 2015 (07:21) 
## Version: 
## last-updated: Mar  5 2018 (19:39) 
##           By: Thomas Alexander Gerds
##     Update #: 18
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
prepareLabels <- function(labels,titles,...){
    labs <- labels
    tits <- titles
    labels <- labs$labels
    titles <- tits$labels
    labs$labels <- NULL
    tits$labels <- NULL
    if (is.matrix(labels)) {
        cnames <- colnames(labels)
        labels <- lapply(1:ncol(labels),function(j)labels[,j])
        names(labels) <- cnames
    }
    if (is.factor(labels) || is.numeric(labels) || is.character(labels)) 
        labels <- list(" "=labels)
    ncolumns <- length(labels)
    if (is.null(titles)){
        titles <- names(labels)
        do.titles <- TRUE
        if (is.null(titles)){
            do.titles <- FALSE
        }
    } else do.titles <- TRUE
    if (do.titles && length(titles)!=length(labels)){
        message(paste("Wrong number of titles: there are",ncolumns,"columns but ",length(titles),"title labels:",paste(titles,collapse=", ")))
    }
    if (length(labs$cex)<ncolumns){
        labs$cex <- rep(labs$cex,length.out=ncolumns)
    }
    if (length(tits$cex)<ncolumns){
        tits$cex <- rep(tits$cex,length.out=ncolumns)
    }
    if (is.null(titles)) titles <- rep(" ",ncolumns)
    if (is.null(labs$interspc))
        labels.interspc <- 1
    else
        labels.interspc <- labs$interspc
    colwidths <- sapply(1:ncolumns,function(f){
                            strwidth("m",units="inches")*labels.interspc +
                                max(strwidth(titles[[f]],cex=tits$cex[[f]],units="inches"),
                                    strwidth(labels[[f]],cex=labs$cex[[f]],units="inches"))
                        })
    if (do.titles==FALSE) titles <- NULL
    list(labels=labels,
         labels.args=labs,
         titles=titles,
         titles.args=tits,
         ncolumns=ncolumns,
         columnwidths=colwidths)
}

#----------------------------------------------------------------------
### prepareLabels.R ends here
