### plotLabels.R --- 
#----------------------------------------------------------------------
## author: Thomas Alexander Gerds
## created: May 11 2015 (09:05) 
## Version: 
## last-updated: May 12 2015 (10:23) 
##           By: Thomas Alexander Gerds
##     Update #: 39
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
plotLabels <- function(labels,titles,width,ylim,stripes,...){
    labs <- labels
    tits <- titles
    labels <- labs$labels
    titles <- tits$labels
    if (is.matrix(labels)) {
        cnames <- colnames(labels)
        labels <- lapply(1:ncol(labels),function(j)labels[,j])
        names(labels) <- cnames
    }
    if (is.factor(labels) || is.numeric(labels) || is.character(labels)) labels <- list(col1=labels)
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
    columnwidths <- sapply(1:ncolumns,function(f){
                               strwidth("m",units="inches")*labels.interspc +
                                   max(strwidth(titles[[f]],cex=tits$cex[[f]],units="inches"),
                                       strwidth(labels[[f]],cex=labs$cex[[f]],units="inches"))
                           })
    ## available space (width) is divided according to relative widths
    labelrelwidth <- columnwidths/sum(columnwidths)
    colwidths <- labelrelwidth*width
    largs <- labs
    if (largs$pos==4)
        xpos <- c(0,cumsum(colwidths)[-ncolumns])
    else
        xpos <- cumsum(colwidths)
    plot(0,0,type="n",axes=FALSE,xlim=c(0,width),ylim=ylim,xlab="",ylab="")
    if (!missing(stripes) && length(stripes)>0){
        stripes$xlim <- c(0,width)
        do.call("stripes",stripes)
    }
    ## arrows(x0=0,x1=width,y0=12,y1=12,lwd=8,col="orange")
    ## abline(v=xpos,col=1:5)
    nix <- lapply(1:ncolumns,function(l){
                      largs$x <- xpos[[l]]
                      largs$labels <- labels[[l]]
                      largs$cex <- largs$cex[[l]]
                      do.call("text",largs)
                  })
    ## to avoid that expression(bold(CI[95])) is
    ## changed to bold(CI[95]) we make titles a list
    if (length(titles)==1) titles <- list(titles)
    if (do.titles){
        ## title.columns <- lapply(1:ncolumns,function(cc){sprintf(fmt=fmt.columns[[cc]],titles[[cc]])})
        tlargs <- tits
        nix <- lapply(1:ncolumns,function(l){
                          tlargs$x <- xpos[[l]]
                          tlargs$labels <- titles[[l]]
                          tlargs$cex <- tlargs$cex[[l]]
                          do.call("text",tlargs)
                      })
    }
}

#----------------------------------------------------------------------
### plotLabels.R ends here
