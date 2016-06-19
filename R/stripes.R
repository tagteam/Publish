### stripes.R --- 
#----------------------------------------------------------------------
## author: Thomas Alexander Gerds
## created: May 12 2015 (06:52) 
## Version: 
## last-updated: Jun 19 2016 (09:19) 
##           By: Thomas Alexander Gerds
##     Update #: 23
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
#' Background and grid color control.
#' 
#' Some users like background colors, and it may be helpful to have grid lines
#' to read off e.g. probabilities from a Kaplan-Meier graph. Both things can be
#' controlled with this function. However, it mainly serves
#' \code{\link{plot.prodlim}}.
#' 
#' 
#' @param xlim Limits for the horizontal x-dimension. Defaults to
#' par("usr")[1:2].
#' @param ylim Limits for the vertical y-dimension.
#' @param col Colors use for the stripes. Can be a vector of colors
#' which are then repeated appropriately.
#' @param lwd Line width 
#' @param gridcol Color of grid lines
#' @param fill Color to fill the background rectangle given by
#' par("usr").
#' @param horizontal Numerical values at which to show horizontal grid
#' lines, and at which to change the color of the stripes.
#' @param vertical Numerical values at which to show vertical grid
#' lines.
#' @param border If a fill color is provided, the color of the border
#' around the background.
#' @param xpd From \code{help(par)}: A logical value or NA.  If FALSE,
#' all plotting is clipped to the plot region, if TRUE, all plotting
#' is clipped to the figure region, and if NA, all plotting is clipped
#' to the device region.  See also \code{clip}.
#' @author Thomas Alexander Gerds <tag@@biostat.ku.dk>
#' @keywords survival
#' @examples
#' 
#' 
#' plot(0,0)
#' backGround(bg="beige",fg="red",vertical=0,horizontal=0)
#' 
#' plot(0,0)
#' stripes(col=c("yellow","green"),gridcol="red",xlim=c(-1,1),horizontal=seq(0,1,.1))
#' stripes(col=c("yellow","green"),gridcol="red",horizontal=seq(0,1,.1))
#' 
#' @export
stripes <- function(xlim,
                    ylim,
                    col="white",
                    lwd=1,
                    gridcol="gray77",
                    fill="white",
                    horizontal=NULL,
                    vertical=NULL,
                    border="black",xpd=FALSE){
    U <- par("usr")
    if (!missing(xlim)){
        U[1] <- xlim[1]
        U[2] <- xlim[2]
    }
    if (!missing(ylim)){
        U[3] <- ylim[1]
        U[4] <- ylim[2]
    }
    # background
    if (!is.null(fill))
        rect(U[1],U[3],U[2],U[4],col=fill, border=border,xpd=xpd) 
    if (!is.null(col)){
        if (length(col)==1){
            rect(U[1],U[3],U[2],U[4],col=col[1], border=border,xpd=xpd)
        }else{
             if (length(col)>1){
                 horizontal
                 NR <- length(horizontal)
                 bcol <- rep(col,length.out=NR)
                 nix <- sapply(1:(NR-1),function(r){
                                     polygon(x=c(U[1],U[1],U[2],U[2],U[1]),
                                             y=c(horizontal[r],horizontal[r+1],horizontal[r+1],horizontal[r],horizontal[r]),
                                             col=bcol[r],
                                             xpd=xpd,
                                             border=FALSE)
                                     ## do NOT specify: density=100 as this slows this down!
                                 })
             }
         }
    }
    # grid
    if (length(gridcol)>0){
        if (length(vertical)>0)
            abline(v=vertical,col=gridcol,xpd=xpd)
        if (length(horizontal)>0){
            ## abline(h=horizontal,col=gridcol,xpd=xpd)
            for (h in horizontal){
                segments(x0=U[1],x1=U[2],y0=h,y1=h,col=gridcol,xpd=xpd,lwd=lwd)
            }
        }
    }
}


#----------------------------------------------------------------------
### stripes.R ends here
