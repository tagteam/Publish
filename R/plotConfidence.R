### plotConfidence.R ---
#-------
## author: Thomas Alexander Gerds
## created: May 10 2015 (11:03)
## Version:
## last-updated: Oct 22 2017 (16:43) 
##           By: Thomas Alexander Gerds
##     Update #: 311
#----------------------------------------------------------------------
##
### Commentary:
##
### Change Log:
#----------------------------------------------------------------------
##
### Code:
##' Function to plot confidence intervals with their values and additional labels.
##' One anticipated use of this function involves first the generation of a regression object,
##' then arrangement of a result table with "regressionTable", further arrangment of table with
##' with e.g. "fixRegressionTable" and various user defined changes - and then finally table
##' along with forest plot using the current function.
##'
##' Function to plot means and other point estimates with confidence intervals,
##' their values and additional labels .
##' Horizonal margins as determined by par()$mar are ignored.
##' Instead layout is used to divide the plotting region horizontally
##' into two or three parts plus leftmargin and rightmargin.
##'
##' When values is FALSE there are only two parts. The default order is
##' labels on the left confidence intervals on the right.
##' When no labels are given or labels is FALSE there are only two parts. The default order is
##' confidence intervals on the left values on the right.
##'
##' The default order of three parts from left to right is
##' labels, confidence intervals, values. The order can be changed as shown
##' by the examples below. The relative widths of the two or three parts
##' need to be adapted to the actual size of the text of the labels. This
##' depends on the plotting device and the size of the font and figures and
##' thus has to be adjusted manually.
##'
##' Oma can be used to further control horizontal margins, e.g., par(oma=c(0,4,0,4)).
##'
##' If confidence limits extend beyond the range determined by xlim, then
##' arrows are drawn at the x-lim borders to indicate that the confidence
##' limits continue.
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
##' @param pch Symbol for points.
##' @param cex Defaults size of all figures and plotting symbol.
##' Single elements are controlled separately. See \code{...}.
##' @param lwd Default width of all lines Single elements are
##' controlled separately. See \code{...}.
##' @param col Default colour of confidence intervals.
##' @param xlim Plotting limits for the confidence intervals. See also
##' \code{xratio} on how to control the layout.
##' @param xlab Label for the x-axis.
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
##' @param order Order of the three blocks: labels, confidence limits,
##' values. See examples.
##' @param leftmargin Percentage of plotting region used for
##' leftmargin. Default is 0.025. See also Details.
##' @param rightmargin Percentage of plotting region used for
##' rightmargin. Default is 0.025. See also Details.
##' @param stripes Vector of up to three Logicals. If \code{TRUE} draw
##' stripes into the background. The first applies to the labels, the
##' second to the graphical presentation of the confidence intervals
##' and the third to the values. Thus, stripes
##' @param factor.reference.pos Position at which factors attain
##' reference values.
##' @param factor.reference.label Label to use at
##' \code{factor.reference.pos} instead of values.
##' @param factor.reference.pch Plotting symbol to use at
##' \code{factor.reference.pos}
##' @param refline Vertical line to indicate the null
##' hypothesis. Default is 1 which would work for odds ratios and
##' hazard ratios.
##' @param xratio One or two values between 0 and 1 which determine
##' how to split the plot window in horizontal x-direction. If there
##' are two blocks (labels, CI) or (CI, values) only one value is used
##' and the default is 0.618 (goldener schnitt) which gives the
##' graphical presentation of the confidence intervals 38.2 % of the
##' graph. The remaining 61.8 % are used for the labels (or values).
##' If there are three blocks (labels, CI, values), xratio has two
##' values which default to fractions of 0.7 according to the relative
##' widths of labels and values, thus by default only 0.3 are used for
##' the graphical presentation of the confidence intervals. The
##' remaining 30 % are used for the graphical presentation of the
##' confidence intervals. See examles
##' @param y.offset Either a single value or a vector determining the
##' vertical offset of all rows.  If it is a single value all rows are
##' shifted up (or down if negative) by this value.  This can be used
##' to add a second group of confidence intervals to an existing graph
##' or to achieve a visual differentiation of rows that belong
##' together.  See examples.
##' @param y.title.offset Numeric value by which to vertically shift
##' the titles of the labels and values.
##' @param digits Number of digits, passed to \code{pubformat} and
##' \code{formatCI}.
##' @param format Format for constructing values of confidence
##' intervals. Defaults to '(u;l)' if there are negative lower or
##' upper values and to '(u-l)' otherwise.
##' @param extremearrows.length Length of the arrows in case of
##' confidence intervals that stretch beyond xlim.
##' @param extremearrows.angle Angle of the arrows in case of
##' confidence intervals that stretch beyond xlim.
##' @param add Logical. If \code{TRUE} do not draw labels or values
##' and add confidence intervals to existing plot.
##' @param layout Logical. If \code{FALSE} do not call layout. This is useful when
##' several plotConfidence results should be combined in one graph and hence layout is called
##' externally.
##' @param xaxis Logical. If \code{FALSE} do not draw x-axis.
##' @param ... Used to control arguments of the following subroutines:
##' \code{plot}: Applies to plotting frame of the graphical
##' presentation of confidence intervals. Use arguments of
##' \code{plot}, e.g., \code{plot.main="Odds ratio"}.  \code{points},
##' \code{arrows}: Use arguments of \code{points} and \code{arrows},
##' respectively. E.g., \code{points.pch=8} and \code{arrows.lwd=2}.
##' \code{refline}: Use arguments of \code{segments}, e.g.,
##' \code{refline.lwd=2}. See \link{segments}.  \code{labels},
##' \code{values}, \code{title.labels}, \code{title.values}: Use
##' arguments of \code{text}, e.g., \code{labels.col="red"} or
##' \code{title.values.cex=1.8}.  \code{xaxis}: Use arguments of
##' \code{axis}, e.g., \code{xaxis.at=c(-0.3,0,0.3)} \code{xlab}: Use
##' arguments of \code{mtext}, e.g., \code{xlab.line=2}.
##' \code{stripes}: Use arguments of \code{stripes}. See examples.
##' See examples for usage.
##' @return List of dimensions and coordinates
##' @examples
##'
##' library(Publish)
##' data(CiTable)
##'
##' ## columns 6, 7, 8, 9 contain the hazard ratio, the lower
##' ## and the upper confidence limits and the p-values, respectively
##' head(CiTable[,6:9])
##'
##' ## columns 1,2,3,4,5 contain the labels
##' head(CiTable[,1:5])
##'
##' ## A first draft version of the plot is obtained as follows
##' plotConfidence(x=CiTable[,6:8], labels=CiTable[,1:5])
##'
##' ## The graph consist of at most three blocks:
##' ##
##' ## block 1: labels
##' ## block 2: printed values of the confidence intervals
##' ## block 3: graphical presentation of the confidence intervals
##' ##
##' ## NOTE: block 3 appears always, the user decides if also
##' ##       blocks 1, 2 should appear
##' ##
##' ## The blocks are arranged with the function layout
##' ## and the default order is 1,3,2 such that the graphical
##' ## display of the confidence intervals appears in the middle
##' ##
##' ## the order of appearance of the three blocks can be changed as follows
##' plotConfidence(x=CiTable[,6:8],
##'                labels=CiTable[,1:5],
##'                order=c(1,3,2))
##' plotConfidence(x=CiTable[,6:8],
##'                labels=CiTable[,1:5],
##'                order=c(2,3,1))
##' ## if there are only two blocks the order is 1, 2
##' plotConfidence(x=CiTable[,6:8],
##'                labels=CiTable[,1:5],
##'                values=FALSE,
##'                order=c(2,1))
##' plotConfidence(x=CiTable[,6:8],
##'                labels=CiTable[,1:5],
##'                values=FALSE,
##'                order=c(1,2))
##'
##' 
##'
##' ## The relative size of the blocks needs to be controlled manually
##' ## by using the argument xratio. If there are only two blocks
##' plotConfidence(x=CiTable[,6:8],
##'                labels=CiTable[,1:5],xratio=c(0.4,0.15))
##'
##' ## The amount of space on the left and right margin can be controlled
##' ## as follows:
##' plotConfidence(x=CiTable[,6:8],labels=CiTable[,1:5],xratio=c(0.4,0.15),
##'                leftmargin=0.1,rightmargin=0.00)
##'
##' ## The actual size of the current graphics device determines
##' ## the size of the figures and the space between them.
##' ## The sizes and line widths are increased as follows:
##' plotConfidence(x=CiTable[,6:8],
##'                xlab="Hazard ratio",
##'                labels=CiTable[,1:5],
##'                cex=1.3,
##'                lwd=3,
##'                xaxis.lwd=1.3,
##'                xaxis.cex=1.3)
##' ## Note that 'cex' of axis ticks is controlled via 'par' but
##' ## cex of the label via argument 'cex' of 'mtext'.
##' ## The sizes and line widths are decreased as follows:
##' plotConfidence(x=CiTable[,6:8],
##'                labels=CiTable[,1:5],
##'                cex=0.8,
##'                lwd=0.8,
##'                xaxis.lwd=0.8,
##'                xaxis.cex=0.8)
##'
##' 
##' ## Another good news is that all figures can be controlled separately
##'
##' ## The size of the graphic device can be controlled in the usual way, e.g.:
##' \dontrun{
##'     pdf("~/tmp/testCI.pdf",width=8,height=8)
##'     plotConfidence(x=CiTable[,6:8],
##'                    labels=CiTable[,1:5])
##'     dev.off()
##' }
##'
##' ## More control of the x-axis and confidence intervals that
##' ## stretch outside the x-range.
##' plotConfidence(x=CiTable[,6:8],
##'                xlab="Hazard ratio",
##'                xlab.line=1.8,
##'                xaxis.at=c(0.8,1,1.3),
##'                labels=CiTable[,1:5],xlim=c(0.8,1.3))
##'
##' ## log-scale
##' plotConfidence(x=CiTable[,6:8],
##'                xlab="Hazard ratio",
##'                xlab.line=1.8,
##'                xaxis.at=c(0.8,1,1.3),
##'                labels=CiTable[,1:5],xlim=c(0.8,1.3),plot.log="x")
##' ## More pronounced arrows
##' ## Coloured xlab expression
##' plotConfidence(x=CiTable[,6:8],
##'                xlab=expression(HR[1](s)),
##'                xlab.line=1.8,
##'                xlab.col="darkred",
##'                extremearrows.angle=50,
##'                extremearrows.length=0.1,
##'                labels=CiTable[,1:5],xlim=c(0.8,1.3))
##'
##' ## Controlling the labels and their titles
##' ## and the values and their titles
##' plotConfidence(x=CiTable[,6:8],
##'                labels=CiTable[,1:5],
##'                xlab="Hazard ratio",
##'                title.values=expression(bold(HR (CI[95]))),
##'                title.labels=c("Drug/Time","Dose","Mean","St.dev.","N"),
##'                factor.reference.pos=c(1,10,19),
##'                cex=1.3,
##'                xaxis.at=c(0.75,1,1.25,1.5,2))
##'
##' ## For factor reference groups, one may want to replace the
##' ## confidence intervals by the word Reference, as in the previous example.
##' ## To change the word 'Reference' we use the argument factor.reference.label:
##' ## To change the plot symbol for the reference lines factor.reference.pch
##' ## To remove the plot symbol use 'NA' as follows:
##' plotConfidence(x=CiTable[,6:8],
##'                labels=CiTable[,1:5],
##'                xlab="Hazard ratio",
##'                factor.reference.label="Ref",
##'                title.values=expression(bold(HR (CI[95]))),
##'                title.labels=c("Drug/Time","Dose","Mean","St.dev.","N"),
##'                factor.reference.pos=c(1,10,19),
##'                factor.reference.pch=NA,
##'                cex=1.3,
##'                xaxis.at=c(0.75,1,1.25,1.5,2))
##'
##'
##' ## changing the style of the graphical confidence intervals
##' plotConfidence(x=CiTable[,6:8],
##'                labels=CiTable[,1:5],
##'                xlab="Hazard ratio",
##'                factor.reference.pos=c(1,10,19),
##'                points.pch=15,
##'                points.col=rainbow(27),
##'                points.cex=2,
##'                arrows.col="darkblue",
##'                cex=1.3,
##'                order=c(1,3,2),
##'                xaxis.at=c(0.75,1,1.25,1.5))
##'
##'
##' ## the values block of the graph can have multiple columns as well
##' ## to illustrate this we create the confidence intervals
##' ## before calling the function and then cbind them
##' ## to the pvalues
##' HR <- pubformat(CiTable[,6])
##' CI95 <- formatCI(lower=CiTable[,7],upper=CiTable[,8],format="(l-u)")
##' pval <- format.pval(CiTable[,9],digits=3,eps=10^{-3})
##' pval[pval=="NA"] <- ""
##' plotConfidence(x=CiTable[,6:8],
##'                labels=CiTable[,1:5],
##'                values=list("HR"=HR,"CI-95"=CI95,"P-value"=pval),
##'                cex=1.2,
##'                xratio=c(0.5,0.3))
##'
##' ## Finally, vertical blocks can be delimited with background color
##' ## NOTE: this may slow things down and potentially create
##' ##       large figures (many bytes)
##' col1 <- rep(c(prodlim::dimColor("green",density=22),
##'               prodlim::dimColor("green")),length.out=9)
##' col2 <- rep(c(prodlim::dimColor("orange",density=22),
##'               prodlim::dimColor("orange")),length.out=9)
##' col3 <- rep(c(prodlim::dimColor("blue",density=22),
##'               prodlim::dimColor("blue")),length.out=9)
##' plotConfidence(x=CiTable[,6:8],
##'                labels=CiTable[,1:5],
##'                stripes=c(1,0,1),
##'                stripes.col=c(col1,col2,col3))
##' plotConfidence(x=CiTable[,6:8],
##'                labels=CiTable[,1:5],
##'                stripes=c(1,1,1),
##'                stripes.col=c(col1,col2,col3))
##'
##' threegreens <- c(prodlim::dimColor("green",density=55),
##'                  prodlim::dimColor("green",density=33),
##'                  prodlim::dimColor("green",density=22))
##' plotConfidence(x=CiTable[,6:8],
##'                labels=CiTable[,1:5],
##'                values=FALSE,
##'                xlim=c(0.75,1.5),
##'                stripes=c(1,1,1),
##'                xratio=c(0.5,0.15),
##'                stripes.horizontal=c(0,9,18,27)+0.5,
##'                stripes.col=threegreens)
##'
##' @export
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
plotConfidence <- function(x,
                           lower,
                           upper,
                           pch=16,
                           cex=1,
                           lwd=1,
                           col=4,
                           xlim,
                           xlab,
                           labels,
                           title.labels,
                           values,
                           title.values,
                           order,
                           leftmargin=0.025,
                           rightmargin=0.025,
                           stripes,
                           factor.reference.pos,
                           factor.reference.label="Reference",
                           factor.reference.pch=8,
                           refline=1,
                           xratio,
                           y.offset=0,
                           y.title.offset=1.3,
                           digits=2,
                           format,
                           extremearrows.length=0.05,
                           extremearrows.angle=30,
                           add=FALSE,
                           layout=TRUE,
                           xaxis=TRUE,
                           ...){
    # {{{ extract confidence data

    if (!is.list(x)) x <- list(x=x)
    m <- x[[1]]

    names(x) <- tolower(names(x))
    if (missing(lower)) {
        lower <- x$lower
    }
    if (missing(upper)) upper <- x$upper
    if (missing(xlim))
        xlim <- c(min(lower)-0.1*min(lower),max(upper)+0.1*min(upper))
    if (missing(xlab)) xlab <- ""
    NR <- length(x[[1]])
    if (!(length(y.offset) %in% c(1,NR))){
        warning(paste("The given",length(y.offset),"many y-offsets are pruned/extended to the length",NR,"lines of the plot."))
    }
    if (length(y.offset)!=NR)
        y.offset <- rep(y.offset,length.out=NR)
    ylim <- c(0,NR+1+y.offset[[length(y.offset)]])
    at <- (1:NR)+y.offset
    rat <- rev(at)
    dimensions <- list("NumberRows"=NR,xlim=xlim,ylim=ylim,ypos=at)

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
    if (do.values==TRUE){
        if (!missing(title.values) && (is.logical(title.values) && title.values[[1]]==FALSE))
            do.title.values <- FALSE
        else
            do.title.values <- TRUE
    }else{
         do.title.values <- FALSE
     }
    if (do.values){
        if (missing(values)){
            if (missing(format))
                if (any(upper<0))
                    format <- "(u;l)"
                else
                    format <- "(u-l)"
            values.defaults <- paste(pubformat(x[[1]],digits=digits),
                                     apply(cbind(lower,upper),
                                           1,
                                           function(x)formatCI(lower=x[1],upper=x[2],format=format,digits=digits)))
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
          title.values <- NULL
      }
    # }}}
    if (add==TRUE) do.values <- do.title.values <- do.labels <- do.title.labels <- FALSE
    # {{{ smart argument control
    dist <- (at[2]-at[1])/2
    if (missing(stripes) || is.null(stripes))
        do.stripes <- FALSE
    else
        do.stripes <- stripes
    stripes.DefaultArgs <- list(col=c(prodlim::dimColor("orange"),"white"),
                                horizontal=seq(min(at)-dist,max(at)+dist,1),
                                xlim=xlim,
                                border=NA)
    if (xaxis) do.xaxis <- TRUE
    xaxis.DefaultArgs <- list(side=1,las=1,pos=0,cex=cex)
    xlab.DefaultArgs <- list(text=xlab,side=1,line=1.5,xpd=NA,cex=cex)
    plot.DefaultArgs <- list(0,0,type="n",ylim=ylim,xlim=xlim,axes=FALSE,ylab="",xlab=xlab)
    points.DefaultArgs <- list(x=m,y=rat,pch=16,cex=cex,col=col,xpd=NA)
    arrows.DefaultArgs <- list(x0=lower,y0=rat,x1=upper,y1=rat,lwd=lwd,col=col,xpd=NA,length=0,code=3,angle=90)
    refline.DefaultArgs <- list(x0=refline,y0=0,x1=refline,y1=max(at),lwd=lwd,col="red",xpd=NA)
    if (missing(labels)) labels <- NULL
    if (missing(title.labels)) title.labels <- NULL
    labels.DefaultArgs <- list(x=0,y=rat,cex=cex,labels=labels,xpd=NA,pos=4)
    title.labels.DefaultArgs <- list(x=0,y=NR+1*y.title.offset + y.offset[length(y.offset)],cex=NULL,labels=title.labels,xpd=NA,font=2,pos=NULL)
    values.DefaultArgs <- list(x=0,y=rat,labels=values.defaults,cex=cex,xpd=NA,pos=4)
    title.values.DefaultArgs <- list(x=0,y=NR+1*y.title.offset+ y.offset[length(y.offset)],labels=title.values,cex=NULL,xpd=NA,font=2,pos=NULL)
    smartA <- prodlim::SmartControl(call=  list(...),
                                    keys=c("plot","points","arrows","refline","labels","values","title.labels","title.values","xaxis","stripes","xlab"),
                                    ignore=c("formula","data","add","col","lty","lwd","ylim","xlim","xlab","ylab","axes","factor.reference.pos","factor.reference.label","extremearrows.angle","extremearrows.length"),
                                    defaults=list("plot"=plot.DefaultArgs,"points"=points.DefaultArgs,"refline"=refline.DefaultArgs,"labels"=labels.DefaultArgs,"title.labels"=title.labels.DefaultArgs,"stripes"=stripes.DefaultArgs,"values"=values.DefaultArgs,"title.values"=title.values.DefaultArgs,"arrows"=arrows.DefaultArgs,"xaxis"=xaxis.DefaultArgs,"xlab"=xlab.DefaultArgs),
                                    forced=list("plot"=list(axes=FALSE,xlab=""),"xaxis"=list(side=1)),
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
    if (add==FALSE){
        oldmar <- par()$mar
        on.exit(par(mar=oldmar))
        par(mar=c(0,0,0,0))
        ## layout
        dsize <- dev.size(units="cm")
        leftmarginwidth <- leftmargin*dsize[1]
        rightmarginwidth <- rightmargin*dsize[1]
        plotwidth <- dsize[1]-leftmarginwidth-rightmarginwidth
        if (do.labels){
            preplabels <- prepareLabels(labels=smartA$labels,
                                        titles=smartA$title.labels)
        }
        if (do.values){
            prepvalues <- prepareLabels(labels=smartA$values,
                                        titles=smartA$title.values)
        }
        if (do.labels){
            ## force label into list, then count label columns
            ## and compute strwidth
            if (do.values){
                ## both values and labels
                do.stripes <- rep(do.stripes,length.out=3)
                names(do.stripes) <- c("labels","ci","values")
                if (missing(xratio)) {
                    lwidth <- sum(preplabels$columnwidth)
                    vwidth <- sum(prepvalues$columnwidth)
                    xratio <- c(lwidth/(lwidth+vwidth)*0.7,vwidth/(lwidth+vwidth)*0.7)
                    ## if (lwidth>vwidth)
                    ## xratio <- c((1-(vwidth/lwidth))*0.7,(vwidth/lwidth)*0.7)
                    ## else
                    ## xratio <- c((1-(lwidth/vwidth))*0.7,(lwidth/vwidth)*0.7)
                    ## xratio <- c(0.5,0.2)
                }
                labelswidth <- plotwidth * xratio[1]
                valueswidth <- plotwidth * xratio[2]
                ciwidth <- plotwidth - labelswidth - valueswidth
                mat <- matrix(c(0,c(1,3,2)[order],0),ncol=5)
                if (!missing(order) && length(order)!=3) order <- rep(order,length.out=3)
                if (layout)
                    layout(mat,width=c(leftmarginwidth,c(labelswidth,ciwidth,valueswidth)[order],rightmarginwidth))
                ## layout.show(n=3)
            } else{
                  ## only labels
                  do.stripes <- rep(do.stripes,length.out=2)
                  names(do.stripes) <- c("labels","ci")
                  if (missing(xratio)) xratio <- 0.618
                  labelswidth <- plotwidth * xratio[1]
                  ciwidth <- plotwidth - labelswidth
                  valueswidth <- 0
                  if (!missing(order) && length(order)!=2) order <- rep(order,length.out=2)
                  mat <- matrix(c(0,c(1,2)[order],0),ncol=4)
                  if (layout)
                      layout(mat,width=c(leftmarginwidth,c(labelswidth,ciwidth)[order],rightmarginwidth))
              }
        } else{
              if (do.values){
                  ## only values
                  do.stripes <- rep(do.stripes,length.out=2)
                  names(do.stripes) <- c("ci","values")
                  if (missing(xratio)) xratio <- 0.618
                  valueswidth <- plotwidth * (1-xratio[1])
                  ciwidth <- plotwidth - valueswidth
                  labelswidth <- 0
                  mat <- matrix(c(0,c(2,1)[order],0),ncol=4)
                  if (!missing(order) && length(order)!=2) order <- rep(order,length.out=2)
                  if (layout)
                      layout(mat,width=c(leftmarginwidth,c(ciwidth,valueswidth)[order],rightmarginwidth))
              }else{
                   # none
                   xratio <- 1
                   ciwidth <- plotwidth
                   do.stripes <- do.stripes[1]
                   names(do.stripes) <- "ci"
                   labelswidth <- 0
                   valueswidth <- 0
                   mat <- matrix(c(0,1,0),ncol=3)
                   if (layout)
                       layout(mat,width=c(leftmarginwidth,ciwidth,rightmarginwidth))
               }
          }
        dimensions <- c(dimensions,list(xratio=xratio,
                                        labelswidth=labelswidth,
                                        valueswidth=valueswidth,
                                        ciwidth=ciwidth,layout=mat))
    }
    # }}}
    # {{{ labels

    if (add==FALSE) par(mar=oldmar*c(1,0,1,0))
    if (do.labels){
        if (do.stripes[["labels"]])
            preplabels <- c(preplabels,list(width=labelswidth,ylim=ylim,stripes=smartA$stripes))
        else
            preplabels <- c(preplabels,list(width=labelswidth,ylim=ylim))
        do.call("plotLabels",preplabels)
    }
    # }}}
    # {{{ values
    if (do.values){
        if (do.stripes[["values"]])
            prepvalues <- c(prepvalues,list(width=valueswidth,ylim=ylim,stripes=smartA$stripes))
        else
            prepvalues <- c(prepvalues,list(width=valueswidth,ylim=ylim))
        do.call("plotLabels",prepvalues)
    }
    # }}}
    # {{{ plot which contains the confidence intervals
    if (add==FALSE){
        do.call("plot",smartA$plot)
        if (do.stripes[["ci"]])
            do.call("stripes",smartA$stripes)
        if (do.xaxis==TRUE){
            oldcexaxis <- par()$cex.axis
            on.exit(par(cex.axis=oldcexaxis))
            par(cex.axis=smartA$xaxis$cex)
            if (is.null(smartA$xaxis$labels))
                do.call("axis",smartA$xaxis)
        }
        do.call("mtext",smartA$xlab)
    }
    # }}}
    # {{{ ref line
    if (add==FALSE){
        do.call("segments",smartA$refline)
    }
    # }}}
    # {{{ point estimates and confidence
    do.call("points",smartA$points)
    ## treat arrows that go beyond the x-limits
    if (smartA$arrows$x0>xlim[2]||smartA$arrows$x1<xlim[1])
        warning("One or several confidence intervals are completely outside xlim. You should adjust xlim.")
    tooHigh <- smartA$arrows$x1>xlim[2]
    tooHigh[is.na(tooHigh)] <- FALSE
    tooLow <- smartA$arrows$x0<xlim[1]
    tooLow[is.na(tooLow)] <- FALSE
    if (any(c(tooHigh,tooLow))){
        if (length(smartA$arrows$angle)<NR)
            smartA$arrows$angle <- rep(smartA$arrows$angle,length.out=NR)
        if (length(smartA$arrows$length)<NR)
            smartA$arrows$length <- rep(smartA$arrows$length,length.out=NR)
        if (length(smartA$arrows$code)<NR)
            smartA$arrows$code <- rep(smartA$arrows$code,length.out=NR)
        if (length(smartA$arrows$col)<NR)
            smartA$arrows$col <- rep(smartA$arrows$col,length.out=NR)
        smartA$arrows$x0 <- pmax(xlim[1],smartA$arrows$x0)
        smartA$arrows$x1 <- pmin(xlim[2],smartA$arrows$x1)
        smartA$arrows$code[tooLow & tooHigh] <- 3
        smartA$arrows$code[tooLow & !tooHigh] <- 1
        smartA$arrows$code[!tooLow & tooHigh] <- 2
        smartA$arrows$angle[tooLow | tooHigh] <- extremearrows.angle
        smartA$arrows$length[tooLow | tooHigh] <- extremearrows.length
        aargs <- smartA$arrows
        for (r in 1:NR){
            aargs$x0 <- smartA$arrows$x0[r]
            aargs$x1 <- smartA$arrows$x1[r]
            aargs$y0 <- smartA$arrows$y0[r]
            aargs$y1 <- smartA$arrows$y1[r]
            aargs$code <- smartA$arrows$code[r]
            aargs$col <- smartA$arrows$col[r]
            aargs$length <- smartA$arrows$length[r]
            aargs$angle <- smartA$arrows$angle[r]
            suppressWarnings(do.call("arrows",aargs))
        }
    } else{
        suppressWarnings(do.call("arrows",smartA$arrows))
    }
    # }}}
    invisible(dimensions)
}
#----------------------------------------------------------------------
### plotResults.R ends here
