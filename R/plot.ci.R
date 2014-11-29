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
##' @param lower lower confidence limits
##' @param upper upper confidence limits
##' @param labels labels 
##' @param label.pos x-position of labels
##' @param title.labels title for label column
##' @param values Logical: if \code{TRUE} show the values of the point
##' estimates and confidence intervals on the graph
##' @param value.pos x-postion of values
##' @param title.values Label for the column (or row) of the values.
##' @param pch Point type for point estimates
##' @param cex Size of points
##' @param lwd Line type for the confidence intervals
##' @param col Color for confidence intervals and point estimates.
##' Can be controlled separately via points.col and segments.col
##' @param xlim Limit of the x-axis
##' @param ylim Limit of the y-axis
##' @param ylab Label for the x-axis
##' @param xlab Label for the y-axis
##' @param digits For rounding of values, passed to format and
##' print.ci
##' @param format A string which indicates the format used for confidence intervals.
##' The string is passed to \code{\link{formatCI}} with two arguments: the lower and the upper
##' limit. For example \code{'(l;u)'} yields confidence intervals with round parenthesis in which
##' the upper and the lower limits are separated by semicolon.
##' @param shift Shift the whole graph \code{shift} units to the right if \code{shift}
##' is positive and to the left if shift is negative. Useful for combining multiple plots.
##' @param add If not \code{FALSE} add to an existing plot.
##' @param axes if FALSE do not add axes
##' @param axis2 if FALSE do not y-axis
##' @param ... Used to transport arguments for the subroutines: \code{"plot"}, \code{"points"}, \code{"segments"}, \code{"labels"}, \code{"values"}, \code{"title.labels"}, \code{"title.values"}, \code{"axis1"}, \code{"axis2"}, \code{"background"}.
##' @examples
##'
##' data(Diabetes)
##' x=ci.mean(bp.2s~frame,data=Diabetes)
##' plot(x)
##' 
#' @S3method plot ci
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
plot.ci <- function(x,
                    lower,
                    upper,
                    labels=TRUE,
                    label.pos,
                    title.labels=TRUE,
                    values=TRUE,
                    value.pos=TRUE,
                    title.values=TRUE,
                    pch=16,
                    cex=1,
                    lwd=2,
                    col=1,
                    xlim,
                    ylim,
                    ylab,
                    xlab,
                    digits=1,
                    format=NULL,
                    shift=0,
                    add=FALSE,
                    axes=TRUE,
                    axis2=FALSE,
                    ...){
    if (missing(format) || is.null(format)) format <- "[u;l]"
    # {{{  extract data
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
    # {{{  labels
    if (title.labels[1]!=FALSE)
        if (title.labels[1]==TRUE)
            if (is.null(x$labels))
                message("no title labels in object")
            else
                title.labels <- colnames(x$labels)
    if (labels[1]!=FALSE)
        if (labels[1]==TRUE)
            if (is.null(x$labels))
                message("no labels in object")
            else
                labels <- x$labels
    if (NCOL(labels)>1){
        labels <- apply(labels,1,paste,collapse=" / ")
        title.labels <- paste(title.labels,collapse=" / ")
    }
    if (values[1]){
        val <- print.ci(x,
                        print=FALSE,
                        digits=digits,
                        format=format,
                        se=FALSE)
        valstring <- paste(val[,2],val[,3])
    }
    if (!is.expression(title.values) && !is.character(title.values) && title.values[1]!=FALSE)
        title.values <- expression(CI[95])
    # }}}
    # {{{  dimensions
    len <- length(m)
    at <- (1:len)
    rat <- rev(at)
    if (missing(xlim))
        xlim <- c(min(lower)-0.1*min(lower),max(upper)+0.1*min(upper))
    if (missing(xlab)) xlab <- ""
    # }}}
    # {{{  default and smart arguments
    background.DefaultArgs <- list(bg="white")
    axis1.DefaultArgs <- list(side=1,las=1)
    axis2.DefaultArgs <- list(side=2,pos=xlim[1],lwd.ticks=0,at=c(0,len),labels=c("",""))
    plot.DefaultArgs <- list(0,0,type="n",ylim=c(0,len+1),xlim=xlim,axes=FALSE,ylab="",xlab=xlab)
    points.DefaultArgs <- list(x=m,y=rat,pch=pch,cex=cex,col=col,xpd=NA)
    segments.DefaultArgs <- list(x0=lower,y0=rat,x1=upper,y1=rat,lwd=lwd,col=col,xpd=NA)
    labels.DefaultArgs <- list(x=xlim[1],
                               y=rat,
                               labels=labels,
                               xpd=NA,
                               pos=2)
    title.labels.DefaultArgs <- list(x=xlim[1],
                                     y=len+1,
                                     labels=title.labels,
                                     xpd=NA,
                                     pos=2)
    values.DefaultArgs <- list(x=xlim[2],
                               y=rat,
                               labels=valstring,
                               xpd=NA,
                               pos=4)
    title.values.DefaultArgs <- list(x=xlim[2],
                                     y=len+1,
                                     labels=title.values,
                                     xpd=NA,
                                     pos=4)
    smartA <- prodlim::SmartControl(call=  list(...),
                                    keys=c("plot","points","segments","labels","values","title.labels","title.values","axis1","axis2","background"),
                                    ignore=c("formula","data","add","col","lty","lwd","ylim","xlim","xlab","ylab","axes","axis2"),
                                    defaults=list("plot"=plot.DefaultArgs,"points"=points.DefaultArgs,"labels"=labels.DefaultArgs,"title.labels"=title.labels.DefaultArgs,"background"=background.DefaultArgs,"values"=values.DefaultArgs,"title.values"=title.values.DefaultArgs,"segments"=segments.DefaultArgs,"axis1"=axis1.DefaultArgs,"axis2"=axis2.DefaultArgs),
                                    forced=list("plot"=list(axes=FALSE),"axis1"=list(side=1)),
                                    verbose=TRUE)
    # }}}
    # {{{  plot and axis
    if (add==FALSE){
        do.call("plot",smartA$plot)
        ## do.call(prodlim::backGround,smartA$background)
    }
    if (axes==TRUE){
        if (is.null(smartA$axis1$labels))
            ## smartA$axis1$labels <- smartA$axis1$at - shift
            do.call("axis",smartA$axis1)
    }
    if (axis2==TRUE)
        do.call("axis",smartA$axis2)
    # }}}
    # {{{  point estimates ci
    do.call("points",smartA$points)
    do.call("segments",smartA$segments)
    # }}}
    # {{{  labels
    if (is.expression(labels) || is.character(labels) || labels[1]!=FALSE)
        do.call("text",smartA$labels)
    if (is.expression(title.labels) || title.labels[1]!=FALSE)
        do.call("text",smartA$title.labels)
    # }}}
    # {{{  values
    if (is.expression(values) || is.character(values) || values[1]!=FALSE)
        do.call("text",smartA$values)
    ## text
    if (is.expression(title.values) || title.values[1]!=FALSE)
        do.call("text",smartA$title.values)
    # }}}
    invisible(smartA)
}
