### plotConfidence.R --- 
#-------
## author: Thomas Alexander Gerds
## created: May 10 2015 (11:03) 
## Version: 
## last-updated: May 11 2015 (15:58) 
##           By: Thomas Alexander Gerds
##     Update #: 78
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
##' Function to plot confidence intervals
##'
##' Function to plot means and other point estimates with confidence intervals
##' @title Plot confidence intervals
##' @param x Either a vector containing the point estimates or A list
##' whose first element contains the point estimates.  Further list
##' elements can contain the confidence intervals and labels. In this
##' case the list needs to have names 'lower' and 'upper' to indicate
##' the values of the lower and the upper limits of the confidence
##' intervals, respectively, and 'labels' to indicate a vector or
##' matrix or list with labels.
##' @param lower Lower confidence limits. Used if object \code{x} is a
##' vector and if \code{x} is a list \code{lower} overwrites element
##' \code{x$lower}.
##' @param upper Upper confidence limits. Used if object \code{x} is a
##' vector and if \code{x} is a list \code{upper} overwrites element
##' \code{x$upper}.
##' @param labels Vector or matrix or list with \code{labels}. Used if
##' object \code{x} is a vector and if \code{x} is a list it
##' overwrites element \code{x$labels}. If \code{labels=FALSE} do not
##' draw labels.
##' @param title.labels Title of the \code{labels}. If \code{labels}
##' is a matrix or list \code{title.labels} should be a vector with as
##' many elements as labels has columns or elements.
##' @param values Either logical or vector, matrix or list with
##' values. If \code{values=TRUE} values are constructed according to
##' \code{format} from \code{lower} and \code{upper} overwrites
##' constructed values. If \code{values=FALSE} do not draw values.
##' @param title.values Title of the \code{values}. If \code{values}
##' is a matrix or list \code{title.labels} should be a vector with as
##' many elements as values has columns or elements.
##' @param factor.reference.pos Position at which factors attain
##' reference values.
##' @param factor.reference.label Label to use at
##' \code{factor.reference.pos} instead of values.
##' @param factor.reference.pch Plotting symbol to use at \code{factor.reference.pos}
##' @param xratio 
##' @param y.offset
##' @param y.title.offset
##' @param pch
##' @param cex
##' @param lwd
##' @param col
##' @param digits
##' @param format
##' @param value.format
##' @param xlim
##' @param xlab
##' @param add
##' @param axes
##' @param ...
##' @return List of coordinates
##' @examples
##' 
##' library(Publish)
##' data(CiTable)
##' plotConfidence(x=CiTable[,6:8],
##'                labels=CiTable[,1:5])
##'
##' 
##' 
##' plotConfidence(x=CiTable[,6:8],
##'                labels=CiTable[,1:5],
##'                xlab="Hazard ratio",
##'                title.values=expression(bold(HR (CI[95]))),
##'                title.labels=c("Drug/Time","Dose","Mean","St.dev.","N"),
##'                factor.reference.pos=c(1,10,19),
##'                cex=1.3,
##'                arrows.length=0.05,
##'                axis1.at=c(0.75,1,1.25,1.5,2))
##' 
##' ## there are three blocks: labels, confidence intervals, values
##' ## the order of appearance can be changed
##' plotConfidence(x=CiTable[,6:8],
##'                labels=CiTable[,1:5],
##'                xlab="Hazard ratio",
##'                factor.reference.pos=c(1,10,19),
##'                cex=1.3,
##'                order=c(1,3,2),
##'                arrows.length=0.05,
##'                axis1.at=c(0.75,1,1.25,1.5))
##'
##' ## changing the style of the graphical confidence intervals
##' plotConfidence(x=CiTable[,6:8],
##'                labels=CiTable[,1:5],
##'                xlab="Hazard ratio",
##'                factor.reference.pos=c(1,10,19),
##'                points.pch=15,
##'                points.cex=2,
##'                arrows.col="darkblue",
##'                cex=1.3,
##'                order=c(1,3,2),
##'                axis1.at=c(0.75,1,1.25,1.5))
##' 
##' 
##' ## the values can have multiple columns as well
##' ## for this we create the confidence intervals
##' ## before calling the function and then cbind them
##' ## to the pvalues
##' CI95 <- formatCI(lower=CiTable[,7],upper=CiTable[,8],format="(u-l)")
##' pval <- format.pval(CiTable[,9],digits=3,eps=10^{-3})
##' pval[pval=="NA"] <- ""
##' plotConfidence(x=CiTable[,6:8],
##'                labels=CiTable[,1:5],
##'                values=list("CI-95"=CI95,"P-value"=pval),
##'                xratio=c(0.4,0.4))
##' 
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
plotConfidence <- function(x,
                           lower,
                           upper,
                           labels,
                           title.labels,
                           values,
                           title.values,
                           order,
                           factor.reference.pos,
                           factor.reference.label="Reference",
                           factor.reference.pch=8,
                           xratio=0.618,
                           y.offset=0,
                           y.title.offset=1.3,
                           pch=16,
                           cex=1,
                           lwd=1,
                           col=1,
                           digits=2,
                           format="(-.-)",
                           value.format,
                           xlim,
                           xlab,
                           add=FALSE,
                           axes=TRUE,
                           ...){
    # {{{ extract confidence data

    if (!is.list(x)) x <- list(x=x)
    m <- x[[1]]
    if (missing(lower)) lower <- x$lower
    if (missing(upper)) upper <- x$upper
    if (missing(xlim))
        xlim <- c(min(lower)-0.1*min(lower),max(upper)+0.1*min(upper))
    if (missing(xlab)) xlab <- ""
    NR <- length(x[[1]])
    ylim <- c(0,NR+1+y.offset[[length(y.offset)]])
    at <- (1:NR)+y.offset
    rat <- rev(at)

    # }}}
    # {{{ preprocessing of labels and title.labels
    if (!missing(labels) && (is.logical(labels) && labels[[1]]==FALSE))
        do.labels <- FALSE
    else
        do.labels <- TRUE
    if (!do.labels || (!missing(title.labels) && (is.logical(title.labels) && title.labels[[1]]==FALSE)))
        do.title.labels <- FALSE
    else
        do.title.labels <- TRUE
    if (do.labels && missing(labels)) {
        labels <- x$labels
        if (is.null(labels)) do.labels <- FALSE
    }
    # }}}
    # {{{ preprocessing of values and confidence intervals 
    if (!missing(values) && (is.logical(values) && values[[1]]==FALSE))
        do.values <- FALSE
    else
        do.values <- TRUE
    if (!missing(title.values) && (is.logical(title.values) && title.values[[1]]==FALSE))
        do.title.values <- FALSE
    else
        do.title.values <- TRUE
    if (do.values){
        if (missing(values)){
            if (missing(value.format))
                if (any(upper<0))
                    value.format <- "(u;l)"
                else
                    value.format <- "(u-l)"
            values.defaults <- paste(pubformat(x[[1]],digits=digits),
                               apply(cbind(x[["lower"]],x[["upper"]]),
                                     1,
                                     function(x)formatCI(lower=x[1],upper=x[2],format=value.format,digits=digits)))
            if (!missing(factor.reference.pos) && is.numeric(factor.reference.pos) && all(factor.reference.pos<length(values.defaults)))
                values.defaults[factor.reference.pos] <- factor.reference.label
            if (do.title.values && (missing(title.values)) || (!is.expression(title.values) && !is.character(title.values)))
                title.values <- expression(bold(CI[95]))
        }else{
             values.defaults <- values
             title.values <- NULL
         }
    } else{
          values.defaults <- NULL
      }

    # }}}
    # {{{ smart argument control 
    background.DefaultArgs <- list(bg="white")
    axis1.DefaultArgs <- list(side=1,las=1,pos=0)
    plot.DefaultArgs <- list(0,0,type="n",ylim=c(0,NR+1+y.offset[length(y.offset)]),xlim=xlim,axes=FALSE,ylab="",xlab=xlab)
    points.DefaultArgs <- list(x=m,y=rat,pch=16,cex=cex,col="blue",xpd=NA)
    arrows.DefaultArgs <- list(x0=lower,y0=rat,x1=upper,y1=rat,lwd=1,col="blue",xpd=NA,length=0,code=3,angle=90)
    if (missing(labels)) labels <- NULL
    if (missing(title.labels)) title.labels <- NULL
    labels.DefaultArgs <- list(x=0,y=rat,cex=cex,labels=labels,xpd=NA,pos=4)
    title.labels.DefaultArgs <- list(x=0,y=NR+1*y.title.offset + y.offset[length(y.offset)],cex=NULL,labels=title.labels,xpd=NA,font=2,pos=NULL)
    values.DefaultArgs <- list(x=0,y=rat,labels=values.defaults,cex=cex,xpd=NA,pos=4)
    title.values.DefaultArgs <- list(x=0,y=NR+1*y.title.offset+ y.offset[length(y.offset)],labels=title.values,cex=NULL,xpd=NA,font=2,pos=NULL)
    smartA <- prodlim::SmartControl(call=  list(...),
                                    keys=c("plot","points","arrows","labels","values","title.labels","title.values","axis1","background"),
                                    ignore=c("formula","data","add","col","lty","lwd","ylim","xlim","xlab","ylab","axes","factor.reference.pos","factor.reference.label"),
                                    defaults=list("plot"=plot.DefaultArgs,"points"=points.DefaultArgs,"labels"=labels.DefaultArgs,"title.labels"=title.labels.DefaultArgs,"background"=background.DefaultArgs,"values"=values.DefaultArgs,"title.values"=title.values.DefaultArgs,"arrows"=arrows.DefaultArgs,"axis1"=axis1.DefaultArgs),
                                    forced=list("plot"=list(axes=FALSE),"axis1"=list(side=1)),
                                    verbose=TRUE)
    if (is.null(smartA$title.labels$pos)) smartA$title.labels$pos <- smartA$labels$pos
    if (is.null(smartA$title.values$pos)) smartA$title.values$pos <- smartA$values$pos
    if (is.null(smartA$title.labels$cex)) smartA$title.labels$cex <- smartA$labels$cex
    if (is.null(smartA$title.values$cex)) smartA$title.values$cex <- smartA$values$cex
    if (!missing(factor.reference.pos) && is.numeric(factor.reference.pos) && all(factor.reference.pos<length(values.defaults))){
        if (length(smartA$points$pch)<NR)
            smartA$points$pch <- rep(smartA$points$pch,length.out=NR)
        smartA$points$pch[factor.reference.pos] <- factor.reference.pch
    }
    # }}}
    # {{{ layout
    oldmar <- par()$mar
    par(mar=c(0,0,0,0))
    ## layout
    dsize <- dev.size(units="cm")
    if (do.labels){
        if (do.values){
            if (missing(xratio)) xratio <- c(0.5,0.3)
            labelswidth <- dsize[1] * xratio[1]
            valueswidth <- dsize[1] * xratio[2]
            ciwidth <- dsize[1] - labelswidth - valueswidth
            mat <- matrix(c(1,3,2),ncol=3)
            layout(mat[,order,drop=FALSE],width=c(labelswidth,valueswidth,ciwidth)[order])
        } else{
              labelswidth <- dsize[1] * xratio
              ciwidth <- dsize[1] - labelswidth
              mat <- matrix(c(1,2),ncol=2)
              layout(mat[order],width=c(labelswidth,ciwidth)[order])
          }
    }
    par(mar=oldmar*c(1,0,1,0))
    # }}}
    # {{{ show labels
    if (do.labels){
        labels.args <- smartA[c("labels","title.labels")]
        names(labels.args) <- c("labels","titles")
        do.call("plotLabels",c(labels.args,list(width=labelswidth,ylim=ylim)))
    }
    ## prodlim::backGround(bg=c("gray77","white"),xlim=dsize,ylim=ylim,horizontal=at,border=NA)
    # }}}
    # {{{ values
    if (do.values){
        values.args <- smartA[c("values","title.values")]
        names(values.args) <- c("labels","titles")
        do.call("plotLabels",c(values.args,list(width=ciwidth,ylim=ylim)))
    }
    ## values.xlim <- c(0,ciwidth)
    ## plot(0,0,type="n",axes=FALSE,xlim=values.xlim,ylim=ylim,xlab="",ylab="")
    ## if (do.values){
    ## do.call("text",smartA$values)
    ## }
    ## text
    ## if (do.title.values){
    ## do.call("text",smartA$title.values)
    ## }
    # }}}
    # {{{ point estimates and confidence
    if (add==FALSE){
        do.call("plot",smartA$plot)
        if (axes==TRUE){
            if (is.null(smartA$axis1$labels))
                do.call("axis",smartA$axis1)
        }
    }
    do.call("points",smartA$points)
    suppressWarnings(do.call("arrows",smartA$arrows))
    # }}}
    par(mar=oldmar)
}
#----------------------------------------------------------------------
### plotResults.R ends here
