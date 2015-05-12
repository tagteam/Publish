### plotConfidence.R --- 
#-------
## author: Thomas Alexander Gerds
## created: May 10 2015 (11:03) 
## Version: 
## last-updated: May 12 2015 (10:23) 
##           By: Thomas Alexander Gerds
##     Update #: 189
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
##' If there are confidence limits outside the range determined by xlim, then
##' arrows are drawn at the border to indicate that the confidence limits continue. 
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
##' @param lwd Default width of all lines
##' Single elements are controlled separately. See \code{...}.
##' @param col Default colour of confidence intervals.
##' @param xlim Plotting limits for the confidence intervals. See also \code{xratio}
##' on how to control the layout.
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
##' @param order Order of the three blocks: labels, confidence limits, values. See examples.
##' @param stripes Vector of up to three Logicals. If \code{TRUE} draw stripes into the background. The
##' first applies to the labels, the second to the graphical presentation of the confidence intervals
##' and the third to the values. Thus, stripes
##' @param factor.reference.pos Position at which factors attain
##' reference values.
##' @param factor.reference.label Label to use at
##' \code{factor.reference.pos} instead of values.
##' @param factor.reference.pch Plotting symbol to use at
##' \code{factor.reference.pos}
##' @param refline Vertical line to indicate the null hypothesis. Default is 1 which
##' would work for odds ratios and hazard ratios.
##' @param xratio One or two values between 0 and 1 which determine
##' how to split the plot window in horizontal x-direction. If there
##' are two blocks (labels, CI) or (CI, values) only one value is used
##' and the default is 0.618 which gives the graphical presentation of
##' the confidence intervals 38.2 % of the graph. The remaining 61.8 %
##' are used for the labels (or values).  If there are three blocks
##' (labels, CI, values), xratio has two values which default to
##' c(0.5,0.2). The remaining 30 % are used for the graphical
##' presentation of the confidence intervals. See examles
##' @param y.offset Either a single value or a vector determining the vertical offset of all rows. 
##' If it is a single value all rows are shifted up (or down if negative) by this value.
##' This can be used to add a second group of confidence intervals to an existing
##' graph or to achieve a visual differentiation of rows that belong together.
##' See examples.
##' @param y.title.offset Numeric value by which to vertically shift the titles of the labels and values.
##' @param digits Number of digits, passed to \code{pubformat} and \code{formatCI}.
##' @param format Format for constructing values of confidence intervals. Defaults to '(u;l)' if there are negative
##' lower or upper values and to '(u-l)' otherwise.
##' @param extremeArrowsLength Length of the arrows in case of confidence intervals that stretch beyond xlim.
##' @param extremeArrowsAngle Angle of the arrows in case of confidence intervals that stretch beyond xlim.
##' @param add Logical. If \code{TRUE} do not draw labels or values and add confidence intervals to existing plot.
##' @param xaxis Logical. If \code{FALSE} do not draw x-axis.
##' @param ... Used to control arguments of the following subroutines:
##' \code{plot}: Applies to plotting frame of the graphical presentation of confidence intervals. Use arguments of \code{plot}, e.g., \code{plot.main="Odds ratio"}.
##' \code{points}, \code{arrows}: Use arguments of \code{points} and \code{arrows}, respectively. E.g., \code{points.pch=8} and \code{arrows.lwd=2}.
##' \code{refline}: Use arguments of \code{segments}, e.g., \code{refline.lwd=2}. See \link{segments}.
##' \code{labels}, \code{values}, \code{title.labels}, \code{title.values}: Use arguments of \code{text}, e.g., \code{labels.col="red"} or \code{title.values.cex=1.8}.
##' \code{xaxis}: Use arguments of \code{axis}, e.g., \code{xaxis.at=c(-0.3,0,0.3)}
##' \code{xlab}: Use arguments of \code{mtext}, e.g., \code{xlab.line=2}.
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
##' ##       blocks 1, 2 shouldappear
##' ## 
##' ## The blocks are arranged with the function layout
##' ## and the default order is 1,3,2 such that the graphical
##' ## display of the confidence intervals is in the middle
##' ## (see Details above)
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
##' ## The relative size of the blocks needs to be controlled manually
##' ## by using the argument xratio. If there are only two blocks 
##' plotConfidence(x=CiTable[,6:8],
##'                labels=CiTable[,1:5],xratio=c(0.4,0.15))
##' 
##' ## The actual size of the current graphics device determines
##' ## the size of the figures and the space between them.
##' ## The sizes and line widths are increased as follows:
##' par(cex.axis=1.3)
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
##' par(cex.axis=0.8)
##' plotConfidence(x=CiTable[,6:8],
##'                labels=CiTable[,1:5],
##'                cex=0.8,
##'                lwd=0.8,
##'                xaxis.lwd=0.8,
##'                xaxis.cex=0.8)
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
##' ## More pronounced arrows
##' ## Coloured xlab expression
##' plotConfidence(x=CiTable[,6:8],
##'                xlab=expression(HR[1](s)),
##'                xlab.line=1.8,
##'                xlab.col="darkred",
##'                extremeArrowsAngle=50,
##'                extremeArrowsLength=0.1,
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
##' CI95 <- formatCI(lower=CiTable[,7],upper=CiTable[,8],format="(u-l)")
##' pval <- format.pval(CiTable[,9],digits=3,eps=10^{-3})
##' pval[pval=="NA"] <- ""
##' plotConfidence(x=CiTable[,6:8],
##'                labels=CiTable[,1:5],
##'                values=list("CI-95"=CI95,"P-value"=pval),
##'                cex=1.2,
##'                xratio=c(0.5,0.3))
##' 
##' ## Finally, vertical blocks can be delimited with background color
##' ## NOTE: this may slow things down and potentially create
##' ##       large figures
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
##'                prodlim::dimColor("green",density=33),
##'                prodlim::dimColor("green",density=22))
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
                           col=1,
                           xlim,
                           xlab,
                           labels,
                           title.labels,
                           values,
                           title.values,
                           order,
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
                           extremeArrowsLength=0.05,
                           extremeArrowsAngle=30,
                           add=FALSE,
                           xaxis=TRUE,
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
                                     apply(cbind(x[["lower"]],x[["upper"]]),
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
    xaxis.DefaultArgs <- list(side=1,las=1,pos=0)
    xlab.DefaultArgs <- list(text=xlab,side=1,line=1.5,xpd=NA,cex=cex)
    plot.DefaultArgs <- list(0,0,type="n",ylim=ylim,xlim=xlim,axes=FALSE,ylab="",xlab=xlab)
    points.DefaultArgs <- list(x=m,y=rat,pch=16,cex=cex,col="blue",xpd=NA)
    arrows.DefaultArgs <- list(x0=lower,y0=rat,x1=upper,y1=rat,lwd=lwd,col="blue",xpd=NA,length=0,code=3,angle=90)
    refline.DefaultArgs <- list(x0=refline,y0=0,x1=refline,y1=max(at),lwd=lwd,col="red",xpd=NA)
    if (missing(labels)) labels <- NULL
    if (missing(title.labels)) title.labels <- NULL
    labels.DefaultArgs <- list(x=0,y=rat,cex=cex,labels=labels,xpd=NA,pos=4)
    title.labels.DefaultArgs <- list(x=0,y=NR+1*y.title.offset + y.offset[length(y.offset)],cex=NULL,labels=title.labels,xpd=NA,font=2,pos=NULL)
    values.DefaultArgs <- list(x=0,y=rat,labels=values.defaults,cex=cex,xpd=NA,pos=4)
    title.values.DefaultArgs <- list(x=0,y=NR+1*y.title.offset+ y.offset[length(y.offset)],labels=title.values,cex=NULL,xpd=NA,font=2,pos=NULL)
    smartA <- prodlim::SmartControl(call=  list(...),
                                    keys=c("plot","points","arrows","refline","labels","values","title.labels","title.values","xaxis","stripes","xlab"),
                                    ignore=c("formula","data","add","col","lty","lwd","ylim","xlim","xlab","ylab","axes","factor.reference.pos","factor.reference.label","extremeArrowsAngle","extremeArrowsLength"),
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
    oldmar <- par()$mar
    par(mar=c(0,0,0,0))
    ## layout
    dsize <- dev.size(units="cm")
    if (do.labels){
        if (do.values){
            ## both values and labels
            do.stripes <- rep(do.stripes,length.out=3)
            if (missing(xratio)) xratio <- c(0.5,0.2)
            labelswidth <- dsize[1] * xratio[1]
            valueswidth <- dsize[1] * xratio[2]
            ciwidth <- dsize[1] - labelswidth - valueswidth
            mat <- matrix(c(1,3,2),ncol=3)
            if (!missing(order) && length(order)!=3) order <- rep(order,length.out=3)
            layout(mat[,order,drop=FALSE],width=c(labelswidth,ciwidth,valueswidth)[order])
            ## layout.show(n=3)
        } else{
              ## only labels
              do.stripes <- rep(do.stripes,length.out=2)
              if (missing(xratio)) xratio <- 0.618
              labelswidth <- dsize[1] * xratio[1]
              ciwidth <- dsize[1] - labelswidth
              valueswidth <- 0
              if (!missing(order) && length(order)!=2) order <- rep(order,length.out=2)
              mat <- matrix(c(1,2),ncol=2)
              layout(mat[,order,drop=FALSE],width=c(labelswidth,ciwidth)[order])
          }
    } else{
          if (do.values){
              ## only values
              do.stripes <- rep(do.stripes,length.out=2)
              if (missing(xratio)) xratio <- 0.618
              valueswidth <- dsize[1] * xratio[1]
              ciwidth <- dsize[1] - valueswidth
              labelswidth <- 0
              mat <- matrix(c(1,2),ncol=2)
              if (!missing(order) && length(order)!=2) order <- rep(order,length.out=2)
              layout(mat[,order,drop=FALSE],width=c(ciwidth,valueswidth)[order])
          }else{
               # none
               labelswidth <- 0
               valueswidth <- 0
           }
      }
    dimensions <- c(dimensions,list(xratio=xratio,
                                    labelswidth=labelswidth,
                                    valueswidth=valueswidth,
                                    ciwidth=ciwidth))
    par(mar=oldmar*c(1,0,1,0))
    # }}}
    # {{{ show labels
    if (do.labels){
        labels.args <- smartA[c("labels","title.labels")]
        names(labels.args) <- c("labels","titles")
        if (do.stripes[[1]])
            labels.args <- c(labels.args,list(width=labelswidth,ylim=ylim,stripes=smartA$stripes))
        else
            labels.args <- c(labels.args,list(width=labelswidth,ylim=ylim))
        do.call("plotLabels",labels.args)
    }
    # }}}
    # {{{ values
    if (do.values){
        values.args <- smartA[c("values","title.values")]
        names(values.args) <- c("labels","titles")
        if (do.stripes[[3]])
            values.args <- c(values.args,list(width=valueswidth,ylim=ylim,stripes=smartA$stripes))
        else
            values.args <- c(values.args,list(width=valueswidth,ylim=ylim))
        do.call("plotLabels",values.args)
    }
    # }}}
    # {{{ plot which contains the confidence intervals
    if (add==FALSE){
        do.call("plot",smartA$plot)
        if (do.stripes[[2]])
            do.call("stripes",smartA$stripes)
        if (do.xaxis==TRUE){
            if (is.null(smartA$xaxis$labels))
                do.call("axis",smartA$xaxis)
        }
        print(smartA$xlab)
        do.call("mtext",smartA$xlab)
    }
    # }}}
    # {{{ ref line
    do.call("segments",smartA$refline)
    # }}}
    # {{{ point estimates and confidence
    do.call("points",smartA$points)
    ## treat arrows that go beyond the x-limits
    tooHigh <- upper>xlim[2]
    tooLow <- lower<xlim[1]
    if (any(c(tooHigh,tooLow))){
        if (length(smartA$arrows$angle)<NR)
            smartA$arrows$angle <- rep(smartA$arrows$angle,length.out=NR)
        if (length(smartA$arrows$length)<NR)
            smartA$arrows$length <- rep(smartA$arrows$length,length.out=NR)
        if (length(smartA$arrows$code)<NR)
            smartA$arrows$code <- rep(smartA$arrows$code,length.out=NR)
        smartA$arrows$x0[tooLow] <- xlim[1]
        smartA$arrows$x1[tooHigh] <- xlim[2]        
        smartA$arrows$code[tooLow & tooHigh] <- 3
        smartA$arrows$code[tooLow & !tooHigh] <- 1
        smartA$arrows$code[!tooLow & tooHigh] <- 2
        smartA$arrows$angle[tooLow | tooHigh] <- extremeArrowsAngle
        smartA$arrows$length[tooLow | tooHigh] <- extremeArrowsLength
        aargs <- smartA$arrows
        for (r in 1:NR){
            aargs$x0 <- smartA$arrows$x0[r]
            aargs$x1 <- smartA$arrows$x1[r]
            aargs$y0 <- smartA$arrows$y0[r]
            aargs$y1 <- smartA$arrows$y1[r]
            aargs$code <- smartA$arrows$code[r]
            aargs$length <- smartA$arrows$length[r]
            aargs$angle <- smartA$arrows$angle[r]
            suppressWarnings(do.call("arrows",aargs))
        }
    } else{
          suppressWarnings(do.call("arrows",smartA$arrows))
      }
    # }}}
    par(mar=oldmar)
    invisible(dimensions)
}
#----------------------------------------------------------------------
### plotResults.R ends here
