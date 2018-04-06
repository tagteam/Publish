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
##' Function to plot means and other point estimates with confidence
##' intervals
##' @title Plot confidence intervals
##' @param x List, data.frame or other object of this form containing point estimates (first element) and the corresponding confidence intervals as elements lower and upper.
##' @param xlim Limit of the x-axis
##' @param xlab Label for the y-axis
##' @param labels labels
##' @param ... Used to transport arguments to \code{plotConfidence}.
##' @examples
##' 
##' data(Diabetes)
##' x=ci.mean(bp.2s~AgeGroups,data=Diabetes)
##' plot(x,title.labels="Age groups",xratio=c(0.4,0.3))
##' x=ci.mean(bp.2s/500~AgeGroups+gender,data=Diabetes)
##' plot(x,xratio=c(0.4,0.2))
##' plot(x,xratio=c(0.4,0.2),
##'      labels=split(x$labels[,"AgeGroups"],x$labels[,"gender"]),
##'      title.labels="Age groups")
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
plot.ci <- function(x,xlim,xlab="",labels,...){
    M <- x[[1]]
    Lower <- x$lower
    Upper <- x$upper
    if (missing(xlim)) xlim <- c(min(Lower),max(Upper))
    if (missing(labels))
        labels <- x$labels
    plotConfidence(list(x=M,lower=Lower,upper=Upper),
                   xlim=xlim,
                   labels=labels,
                   xlab=xlab,
                   ...)
}

