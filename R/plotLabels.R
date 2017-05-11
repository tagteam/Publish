### plotLabels.R --- 
#----------------------------------------------------------------------
## author: Thomas Alexander Gerds
## created: May 11 2015 (09:05) 
## Version: 
## last-updated: Apr 14 2017 (14:01) 
##           By: Thomas Alexander Gerds
##     Update #: 59
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
plotLabels <- function(labels,
                       labels.args,
                       titles,
                       titles.args,
                       width,
                       ylim,
                       ncolumns,
                       columnwidths,
                       ## xpos,
                       stripes,
                       ...){
    ## available space (width) is divided according to relative widths
    labelrelwidth <- columnwidths/sum(columnwidths)
    colwidths <- labelrelwidth*width
    if (labels.args$pos==4)
        ## aligned on right hand
        xpos <- c(0,cumsum(colwidths)[-ncolumns])
    else
        ## aligned on left hand
        xpos <- cumsum(colwidths)
    ## empty plot
    plot(0,0,type="n",axes=FALSE,xlim=c(0,width),ylim=ylim,xlab="",ylab="")
    if (!missing(stripes) && length(stripes)>0){
        stripes$xlim <- c(0,width)
        do.call("stripes",stripes)
    }
    
    ## arrows(x0=0,x1=width,y0=12,y1=12,lwd=8,col="orange")
    ## abline(v=xpos,col=1:5)
    nix <- lapply(1:ncolumns,function(l){
                      labels.args$x <- xpos[[l]]
                      labels.args$labels <- labels[[l]]
                      labels.args$cex <- labels.args$cex[[l]]
                      do.call("text",labels.args)
                  })
    ## to avoid that expression(bold(CI[95])) is
    ## changed to bold(CI[95]) we make titles a list
    if (length(titles)==1) titles <- list(titles)
    if (length(titles)>0){
        ## title.columns <- lapply(1:ncolumns,function(cc){sprintf(fmt=fmt.columns[[cc]],titles[[cc]])})
        nix <- lapply(1:ncolumns,function(l){
                          titles.args$x <- xpos[[l]]
                          titles.args$labels <- titles[[l]]
                          titles.args$cex <- titles.args$cex[[l]]
                          do.call("text",titles.args)
                      })
    }
}

#----------------------------------------------------------------------
### plotLabels.R ends here
