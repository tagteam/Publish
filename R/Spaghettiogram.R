# {{{ header
##' A spaghettiogram is showing repeated measures (longitudinal data)
##'
##' 
##' @title Spaghettiogram
##' @param formula A formula which specifies the variables for the
##' spaghettiograms. If Y ~ X + id(Z) then for each value of Z the
##' spaghettiogram is the graph (X,Y) in the subset defined by the
##' value of Z
##' @param data data set in which variable
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
##'                data=SpaceT,
##'                xlim=c(-0.5,1.5),
##'                axis1.at=c(0,1),
##'                axis1.lab=c("pre","post"))
##' @export
Spaghettiogram <- function(formula,
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
    ## id <- function(x)x
    sf <- specialFrame(cl,
                       special="id",
                       specialsFactor=TRUE,
                       dropIntercept=TRUE)
    if (NCOL(sf$X)>1) stop("Can only handle one x-variable, formula must have the form: y~ x+id(z) where\ny is a measurement\nx tells when the measurement was taken\nand z identifies repeated measurements of the same subject. ")
    XY <- data.frame(cbind(X=sf$design,Y=sf$response))
    names(XY) <- c("X","Y")
    object <- split(XY,sf$id)
    xvar="X"
    yvar="Y"
    ## ff <- as.character(formula)
    ## m <- model.frame(formula,data)
    ## v <- all.vars(formula)
    # }}}
    # {{{ resolve line type and color
    nlines <- length(object)
    if (missing(xlim)) xlim <- range(XY[,"X"])
    if (missing(ylim)) ylim <- range(XY[,"Y"])
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
    axis1.DefaultArgs <- list(side=1,las=1)
    axis2.DefaultArgs <- list(side=2,las=2)
    background.DefaultArgs <- list(bg="white")
    lines.DefaultArgs <- list(type="b",cex=1.3)
    ## text.DefaultArgs <- list(cex=1.4,x=xlim[1],y=ylim[2],pos=3,offset=2,xpd=NA)
    ## mtext.DefaultArgs <- list(cex=1.4,xpd=NA,text="",line=2,cex=2,las=1)
    plot.DefaultArgs <- list(x=0,y=0,type = "n",ylim = ylim,xlim = xlim,xlab = xlab,ylab = ylab)
    legend.DefaultArgs <- list(legend=names(object),lwd=2,col=col,lty=lty,cex=1.5,bty="n",y.intersp=1.3,x="topright")
    smartA <- prodlim::SmartControl(call=  list(...),
                                    keys=c("plot","lines","legend","background","axis1","axis2"),
                                    ignore=c("formula","data","add","col","lty","lwd","ylim","xlim","xlab","ylab","legend","axes","background"),
                                    defaults=list("plot"=plot.DefaultArgs,"lines"=lines.DefaultArgs,"legend"=legend.DefaultArgs,"background"=background.DefaultArgs,"axis1"=axis1.DefaultArgs,"axis2"=axis2.DefaultArgs),
                                    forced=list("plot"=list(axes=FALSE),"axis1"=list(side=1)),
                                    verbose=TRUE)
    ## print(smartA)
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
    ## browser()
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
        tvar <- a[,xvar]
        ## a <- a[!is.na(tvar),]
        do.call("lines",c(list(x=tvar,
                               y=a[,yvar],
                               pch=pch[i],
                               col=col[i],
                               lty=lty[i],
                               lwd=lwd[i]),smartA$lines))
        do.call("lines",
                c(list(x=tvar,y=a[,yvar],pch=pch[i],col=col[i],lty=lty[i],lwd=lwd[i]),
                  replace(smartA$lines,"type","l")))
    })
    # }}}
    invisible(object)
}


