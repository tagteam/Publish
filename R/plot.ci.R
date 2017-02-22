##  ------------------------------------------------------------------
##  _____          _____
## |_   _|_ _  __ |_   _|__  __ _ _ __ ___
##   | |/ _` |/ _` || |/ _ \/ _` | '_ ` _ \
##   | | (_| | (_| || |  __/ (_| | | | | | |
##   |_|\__,_|\__, ||_|\___|\__,_|_| |_| |_|
##            |___/
##  ------------------------------------------------------------------
##' Function to plot confidence intervals
##'
##' Function to plot means and other point estimates with confidence intervals
##' @title Plot confidence intervals
##' @param x Object containing point estimates and the corresponding
##' confidence intervals
##' @param y.offset Adjustment of position of confidence intervals and
##' text on y-axis. When \code{y.offset=0} the positions are integers
##' between the number of intervals (upper most) and 1 (lowest
##' interval).
##' @param y.title.offset Vertical offset for title line
##' @param lower lower confidence limits
##' @param upper upper confidence limits
##' @param labels labels
##' @param title.labels title for label column
##' @param values Logical: if \code{TRUE} show the values of the point
##' estimates and confidence intervals on the graph
##' @param title.values Label for the column (or row) of the values.
##' @param factor.reference.pos Position of factor references.
##' @param factor.reference.label Label to use at factor.reference.pos instead of value.
##' @param pch Point type for point estimates
##' @param cex Size of points
##' @param lwd Line type for the confidence intervals
##' @param col Color for confidence intervals and point estimates.
##' Can be controlled separately via points.col and segments.col
##' @param xlim Limit of the x-axis
##' @param ylim Limit of the y-axis
##' @param ylab Label for the x-axis
##' @param xlab Label for the y-axis
##' @param automar If \code{TRUE} set margin elements 2 and 4 of
##' \code{par()$mar} based on string width of labels and values.
##' @param leftmargin If \code{automar} is \code{TRUE} additional
##' offset -- measured in margin lines -- which is added to left margin.
##' @param rightmargin If \code{automar} is \code{TRUE} additional
##' offset -- measured in margin lines -- which is added to right margin.
##' @param labels.colintersp Interspace between label columns
##' @param digits For rounding of values, passed to \code{pubformat} and \code{print.ci}
##' @param format A string which indicates the format used for
##' confidence intervals.  The string is passed to
##' \code{\link{formatCI}} with two arguments: the lower and the upper
##' limit. For example \code{'(l;u)'} yields confidence intervals with
##' round parenthesis in which the upper and the lower limits are
##' separated by semicolon.
##' @param shift Shift the whole graph \code{shift} units to the right
##' if \code{shift} is positive and to the left if shift is
##' negative. Useful for combining multiple plots.
##' @param add If not \code{FALSE} add to an existing plot.
##' @param axes if FALSE do not add axes
##' @param axis2 if FALSE do not y-axis
##' @param ... Used to transport arguments for the following
##' subroutines: \code{"plot"}, \code{"points"}, \code{"segments"},
##' \code{"labels"}, \code{"values"}, \code{"title.labels"},
##' \code{"title.values"}, \code{"axis1"}, \code{"axis2"},
##' \code{"background"}.
##' @examples
##' 
##' data(Diabetes)
##' x=ci.mean(bp.2s~frame,data=Diabetes)
##' \dontrun{
##' plot(x, leftmargin=0, rightmargin=0)
##' plotConfidence(x, leftmargin=0, rightmargin=0)
##' 
##' data(CiTable)
##' with(CiTable,plotConfidence(x=list(HazardRatio),
##'                                lower=lower,
##'                                upper=upper,
##'                                labels=CiTable[,2:6],
##'                                factor.reference.pos=c(1,10,19),
##'                                format="(u-l)",
##'                                points.col="blue",
##'                                digits=2))
##' 
##' with(CiTable,Publish::plot.ci(x=list(HazardRatio),
##'                                lower=lower,
##'                                upper=upper,
##'                                labels=CiTable[,2:6],
##'                                factor.reference.pos=c(1,10,19),
##'                                format="(u-l)",
##'                                points.col="blue",
##'                                digits=2,
##'                                leftmargin=-2,
##'                                title.labels.cex=1.1,
##'                                labels.cex=0.8,values.cex=0.8))
##' }
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
##' @export
plot.ci <- function(x,
                    y.offset=0,
                    y.title.offset=1.3,
                    lower,
                    upper,
                    labels,
                    title.labels,
                    values,
                    title.values,
                    factor.reference.pos,
                    factor.reference.label="Reference",
                    pch=16,
                    cex=1,
                    lwd=2,
                    col=1,
                    xlim,
                    ylim,
                    ylab,
                    xlab,
                    automar=TRUE,
                    leftmargin=0.25,
                    rightmargin=0.25,
                    labels.colintersp=1,
                    digits=1,
                    format=NULL,
                    shift=0,
                    add=FALSE,
                    axes=TRUE,
                    axis2=FALSE,
                    ...){
    if (missing(format) || is.null(format)) format <- "[u;l]"
    # {{{ extract data
    if (!is.list(x))
        x <- list(x=x)
    m <- x[[1]]+shift
    if (!missing(lower)) x$lower <- lower
    if (!missing(upper)) x$upper <- upper
    if (missing(lower))
        lower <- x$lower+shift
    else
        lower <- lower+shift
    if (missing(upper))
        upper <- x$upper+shift
    else
        upper <- upper+shift
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
        valstring <- paste(pubformat(x[[1]],digits=digits),
                           apply(cbind(x[["lower"]],x[["upper"]]),
                                 1,
                                 function(x)formatCI(lower=x[1],upper=x[2],format=format,digits=digits)))
        if (!missing(factor.reference.pos) && is.numeric(factor.reference.pos) && all(factor.reference.pos<length(valstring)))
            valstring[factor.reference.pos] <- factor.reference.label
    } else{
          valstring <- NULL
      }
    if (do.title.values && (missing(title.values)) || (!is.expression(title.values) && !is.character(title.values)))
        title.values <- expression(bold(CI[95]))

    # }}}
    # {{{ x- and y-dimensions for confidence intervals
    len <- length(m)
    at <- (1:len)+y.offset
    rat <- rev(at)
    if (missing(xlim))
        xlim <- c(min(lower)-0.1*min(lower),max(upper)+0.1*min(upper))
    if (missing(xlab)) xlab <- ""
    # }}}
    # {{{ default and smart arguments
    background.DefaultArgs <- list(bg="white")
    axis1.DefaultArgs <- list(side=1,las=1)
    axis2.DefaultArgs <- list(side=2,pos=xlim[1],lwd.ticks=0,at=c(0,len),labels=c("",""))
    plot.DefaultArgs <- list(0,0,type="n",ylim=c(0,len+1+y.offset[length(y.offset)]),xlim=xlim,axes=FALSE,ylab="",xlab=xlab)
    points.DefaultArgs <- list(x=m,y=rat,pch=pch,cex=cex,col=col,xpd=NA)
    segments.DefaultArgs <- list(x0=lower,y0=rat,x1=upper,y1=rat,lwd=lwd,col=col,xpd=NA)
    if (missing(labels)) labels <- NULL
    if (missing(title.labels)) title.labels <- NULL
    labels.DefaultArgs <- list(x=xlim[1],y=rat,cex=cex,labels=labels,xpd=NA,pos=4)
    title.labels.DefaultArgs <- list(x=xlim[1],y=len+1*y.title.offset + y.offset[length(y.offset)],cex=cex,labels=title.labels,xpd=NA,font=2,pos=NULL)
    values.DefaultArgs <- list(x=xlim[2],y=rat,labels=valstring,cex=cex,xpd=NA,pos=4)
    title.values.DefaultArgs <- list(x=xlim[2],y=len+1*y.title.offset+ y.offset[length(y.offset)],labels=title.values,cex=cex,xpd=NA,font=2,pos=NULL)
    smartA <- prodlim::SmartControl(call=  list(...),
                                    keys=c("plot","points","segments","labels","values","title.labels","title.values","axis1","axis2","background"),
                                    ignore=c("formula","data","add","col","lty","lwd","ylim","xlim","xlab","ylab","axes","axis2","factor.reference.pos","factor.reference.label"),
                                    defaults=list("plot"=plot.DefaultArgs,"points"=points.DefaultArgs,"labels"=labels.DefaultArgs,"title.labels"=title.labels.DefaultArgs,"background"=background.DefaultArgs,"values"=values.DefaultArgs,"title.values"=title.values.DefaultArgs,"segments"=segments.DefaultArgs,"axis1"=axis1.DefaultArgs,"axis2"=axis2.DefaultArgs),
                                    forced=list("plot"=list(axes=FALSE),"axis1"=list(side=1)),
                                    verbose=TRUE)
    if (is.null(smartA$title.labels$pos)) smartA$title.labels$pos <- smartA$labels$pos
    if (is.null(smartA$title.values$pos)) smartA$title.values$pos <- smartA$values$pos

    # }}}
    # {{{ force labels to list
    labels <- smartA$labels$labels
    if (do.labels){
        if (is.matrix(labels)) {
            cnames <- colnames(labels)
            labels <- lapply(1:ncol(labels),function(j)labels[,j])
            names(labels) <- cnames
        }
        if (is.factor(labels) || is.numeric(labels) || is.character(labels)) labels <- list(col1=labels)
        ncolumns <- length(labels)
    }
    title.labels <- smartA$title.labels$labels
    if (do.title.labels && is.null(title.labels)){
        title.labels <- names(labels)
        if (is.null(title.labels)){
            do.title.labels <- FALSE
        }
    }
    if (do.title.labels && length(title.labels)!=length(labels)){
        message(paste("Wrong number of title.labels: there are",ncolumns,"columns but ",length(title.labels),"title labels:",paste(title.labels,collapse=", ")))
    }
    # }}}
    # {{{ left margin
    ## plot.new()
    inches2lines <- (par("mar") / par("mai"))[1]
    if (do.labels){
        if (length(smartA$labels$cex)<ncolumns){
            smartA$labels$cex <- rep(smartA$labels$cex,length.out=ncolumns)
        }
        if (length(smartA$title.labels$cex)<ncolumns){
            smartA$title.labels$cex <- rep(smartA$title.labels$cex,length.out=ncolumns)
        }
        if (is.null(title.labels)) title.labels <- rep(" ",ncolumns)
        columnwidths <- sapply(1:ncolumns,function(f){
                                   strwidth("m",units="inches")*labels.colintersp +
                                       max(strwidth(title.labels[[f]],cex=smartA$title.labels$cex[[f]],units="inches"),
                                           strwidth(labels[[f]],cex=smartA$labels$cex[[f]],units="inches"))
                               })
        labelstextwidth <- sum(columnwidths)
        leftMargin <- leftmargin+labelstextwidth*inches2lines
    }else{
         leftMargin <- leftmargin
     }
    # }}}
    # {{{ right margin
    if (do.values){
        valuestextwidth <- max(strwidth(smartA$values$labels,cex=smartA$values$cex, units="inches"),
                               strwidth(smartA$title.values$labels,cex=smartA$title.values$cex, units="inches"))
        rightMargin <- rightmargin+valuestextwidth *inches2lines
    }else{
         rightMargin <- rightmargin
     }
    if (missing(automar) || automar==TRUE){
        oldmar <- par()$mar
        newmar <- par()$mar + c(0,leftMargin,0,rightMargin)
        par(mar=newmar)
    }
    # }}}
    # {{{ plot and axis

    if (add==FALSE){
        do.call("plot",smartA$plot)
        ## do.call(prodlim::backGround,smartA$background)
    }
    if (missing(automar) || automar==TRUE) par(mar=oldmar) ## reset

    if (axes==TRUE){
        if (is.null(smartA$axis1$labels))
            ## smartA$axis1$labels <- smartA$axis1$at - shift
            do.call("axis",smartA$axis1)
    }
    if (axis2==TRUE)
        do.call("axis",smartA$axis2)
    # }}}
    # {{{ point estimates ci
    do.call("points",smartA$points)
    do.call("segments",smartA$segments)
    # }}}
    # {{{ labels

    if (do.labels){
        ## multiple label columns
        ## if (do.title.labels)
        ## labwidths <- sapply(1:ncolumns,function(cc){max(nchar(labels))})
        ## else
        ## labwidths <- sapply(labels,function(x)max(nchar(as.character(x))))
        columnwidths.usr <- sapply(1:ncolumns,function(f){
                                       strwidth("m",units="inches")*labels.colintersp +
                                           max(strwidth(title.labels[[f]],cex=smartA$title.labels$cex[[f]],units="user"),
                                               strwidth(labels[[f]],cex=smartA$labels$cex[[f]],units="user"))
                                   })
        ## fmt.columns <- paste0("%-",labwidths,"s")
        ## columns <- lapply(1:ncolumns,function(cc){sprintf(fmt=fmt.columns[[cc]],labels[[cc]])})
        largs <- smartA$labels
        if (largs$pos==2)
            xpos <- c(0,rev(cumsum(rev(columnwidths.usr)))[-ncolumns])
        else
            xpos <- rev(cumsum(rev(columnwidths.usr)))
        nix <- lapply(1:ncolumns,function(l){
                          largs$x <- largs$x-xpos[[l]]
                          largs$labels <- labels[[l]]
                          largs$cex <- largs$cex[[l]]
                          do.call("text",largs)
                      })
        if (do.title.labels){
            ## title.columns <- lapply(1:ncolumns,function(cc){sprintf(fmt=fmt.columns[[cc]],title.labels[[cc]])})
            tlargs <- smartA$title.labels
            nix <- lapply(1:ncolumns,function(l){
                              tlargs$x <- tlargs$x-xpos[[l]]
                              tlargs$labels <- title.labels[[l]]
                              tlargs$cex <- tlargs$cex[[l]]
                              do.call("text",tlargs)
                          })
        }
    }

    # }}}
    # {{{ values
    if (do.values){
        do.call("text",smartA$values)
    }
    ## text
    if (do.title.values){
        do.call("text",smartA$title.values)
    }
    # }}}
    invisible(smartA)
}

