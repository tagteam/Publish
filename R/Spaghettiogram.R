# {{{ header
##' A spaghettiogram is showing repeated measures (longitudinal data)
##'
##' 
##' @title Spaghettiogram
##' @aliases spaghettiogram Spaghettiogram
##' @param formula A formula which specifies the variables for the
##' spaghettiograms. If Y ~ X + id(Z) then for each value of Z the
##' spaghettiogram is the graph (X,Y) in the subset defined by the
##' value of Z. Data are expected to be in the "long" format. Y is
##' a numeric vector and X is a factor whose levels define the X-axis.
##' Each level of the id-vector corresponds to 
##' one line (spaghetti) in the plot.
##' 
##' @param data data set in which variables X, Y and Z are defined.
##' @param xlim Limits for x-axis
##' @param ylim Limits for y-axis
##' @param xlab Label for x-axis
##' @param ylab Label for x-axis
##' @param axes Logical indicating if axes should be drawn.
##' @param col Colors for the spaghettiograms
##' @param lwd Widths for the spaghettiograms
##' @param lty Type for the spaghettiograms
##' @param pch Point-type for the spaghettiograms
##' @param legend If \code{TRUE} add a legend. Argument A of legend is
##' controlled as legend.A. E.g., when \code{legend.cex=2} legend will
##' be called with argument cex=2.
##' @param add If \code{TRUE} add to existing plot device.
##' @param background Control the background color of the graph.
##' @param ... used to transport arguments which are passed to the
##' following subroutines: \code{"plot"}, \code{"lines"},
##' \code{"legend"}, \code{"background"}, \code{"axis1"},
##' \code{"axis2"}.
##' @return List with data of each subject
##' @examples
##' 
##' data(SpaceT)
##' Spaghettiogram(HR~Status+id(ID),
##'                data=SpaceT)
##' @export
spaghettiogram <- function(formula,
                            data,
                            xlim,
                            ylim,
                            xlab="",
                            ylab="",
                            axes=TRUE,
                            col,
                            lwd,
                            lty,
                            pch,
                            legend=FALSE,
                            add=FALSE,
                            background=TRUE,
                            ...){
    # {{{ read formula and split data
    cl <- match.call(expand.dots=TRUE)
    sf <- specialFrame(formula,
                       data,
                       unspecials.design=FALSE,
                       specials=c("id"),
                       strip.specials=c("id"),
                       specials.factor=TRUE,
                       specials.design=FALSE,
                       drop.intercept=TRUE)
    ## sf <- specialFrame(cl,
    ## special="id",
    ## specials.factor=TRUE,
    ## drop.intercept=TRUE)
    ## if (NCOL(X)!=1||NCOL(Y)!=1||NCOL(Y)!=1) stop("Can only handle one x-variable, one y-variable and one z-variable, formula must have the form: y~ x + id(z) where\ny is a measurement\nx tells when the measurement was taken\nand z identifies repeated measurements of the same subject. ")
    X <- sf$design[[1]]
    Y <- sf$response[[1]]
    if (missing(ylab))
        ylab <- names(sf$response)[1]
    Z <- sf$id[[1]]
    if (!is.numeric(Y)) {
        if (is.factor(Y)){
            ylevs <- levels(Y)
            Y <- as.numeric(Y)
        } else{
            Y <- factor(Y)
            ylevs <- levels(Y)
            Y <- as.numeric(Y)
        }
    }else{
        ylevs <- NULL
    }
    if (is.numeric(X)){
        xat <- sort(unique(X))
        xlevs <- as.character(xat)
    }else{
        if (!is.factor(X)) X <- factor(X)
        xlevs <- levels(X)
        ## now values are 1= xlev[1], 2= xlev[2], etc.
        X <- as.numeric(X) 
        xat <- sort(unique(X))
    }
    XY <- data.frame(cbind(X=X,Y=Y))
    ## names(XY) <- c("X","Y")
    object <- split(XY,Z)
    # }}}
    # {{{ resolve line type and color
    nlines <- length(object)
    if (missing(xlim)) xlim <- range(xat)
    if (missing(ylim)) ylim <- range(Y)
    if (missing(lwd)) lwd <- rep(3,nlines)
    if (missing(col)) col <- 1:nlines
    if (missing(lty)) lty <- rep(1, nlines)
    if (missing(pch)) pch <- rep(1, nlines)
    if (length(lwd) < nlines) lwd <- rep(lwd, nlines)
    if (length(lty) < nlines) lty <- rep(lty, nlines)
    if (length(col) < nlines) col <- rep(col, nlines)
    if (length(pch) < nlines) pch <- rep(pch, nlines)
    # }}}
    # {{{ processing graphical arguments
    axis1.DefaultArgs <- list(side=1,las=1,at=xat,lab=xlevs)
    axis2.DefaultArgs <- list(side=2,las=2)
    background.DefaultArgs <- list(bg="white")
    lines.DefaultArgs <- list(type="b",cex=1.3)
    ## text.DefaultArgs <- list(cex=1.4,x=xlim[1],y=ylim[2],pos=3,offset=2,xpd=NA)
    ## mtext.DefaultArgs <- list(cex=1.4,xpd=NA,text="",line=2,cex=2,las=1)
    plot.DefaultArgs <- list(x=0,y=0,type = "n",ylim = ylim,xlim = xlim,xlab = xlab,ylab = ylab)
    legend.DefaultArgs <- list(legend=names(object),title=names(sf$id),lwd=2,col=col,lty=lty,cex=1.5,bty="n",y.intersp=1.3,x="topright")
    smartA <- prodlim::SmartControl(call=  list(...),
                                    keys=c("plot","lines","legend","background","axis1","axis2"),
                                    ignore=c("formula","data","add","col","lty","lwd","ylim","xlim","xlab","ylab","legend","axes","background"),
                                    defaults=list("plot"=plot.DefaultArgs,"lines"=lines.DefaultArgs,"legend"=legend.DefaultArgs,"background"=background.DefaultArgs,"axis1"=axis1.DefaultArgs,"axis2"=axis2.DefaultArgs),
                                    forced=list("plot"=list(axes=FALSE),"axis1"=list(side=1)),
                                    verbose=TRUE)
    # }}}
    # {{{ empty plot, background
    if (add==FALSE){
        do.call("plot",smartA$plot)
        if (background)
            do.call(prodlim::backGround,smartA$background)
    }
    # }}}
    # {{{ axes
    if (!add) {
        if (axes){
            do.call("axis",smartA$axis1)
            do.call("axis",smartA$axis2)
        }
    }
    # }}}
    # {{{ text
    ## if (text) do.call("text",smartA$text)
    # }}}
    # {{{ mtext
    ## do.call("mtext",smartA$mtext)
    # }}}
    # {{{ legend
    if (legend)
        do.call("legend",smartA$legend)
    # }}}
    # {{{ adding spaghetti's
    nix <- sapply(1:length(object),function(i){
        a=object[[i]]
        data.table::setDT(a)
        setkey(a,X)
        a <- na.omit(a)
        do.call("lines",c(list(x=a[["X"]],
                               y=a[["Y"]],
                               pch=pch[i],
                               col=col[i],
                               lty=lty[i],
                               lwd=lwd[i]),smartA$lines))
        do.call("lines",
                c(list(x=a[["X"]],
                       y=a[["Y"]],
                       pch=pch[i],
                       col=col[i],
                       lty=lty[i],
                       lwd=lwd[i]),
                  replace(smartA$lines,"type","l")))
    })
    # }}}
    invisible(object)
}
##' @export
Spaghettiogram <- spaghettiogram


