### splinePlot.lrm.R ---
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Dec 31 2017 (11:04)
## Version: 1
## Last-Updated: Feb  8 2018 (12:59) 
##           By: Thomas Alexander Gerds
##     Update #: 13
#----------------------------------------------------------------------
##
### Commentary:
##
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
##' Plotting the prediction of a logistic regression model
##' with confidence bands against one continuous variable. 
##'
##' Function which extracts from a logistic regression model
##' fitted with \code{rms::lrm} the predicted risks or odds.
##' @title Plot predictions of logistic regression 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
##' @param object Logistic regression model fitted with \code{rms::lrm}
##' @param xvar Name of the variable to show on x-axis
##' @param xvalues Sequence of \code{xvar} values 
##' @param xlim x-axis limits
##' @param ylim y-axis limits
##' @param xlab x-axis labels
##' @param ylab y-axis labels
##' @param col color of the line
##' @param lwd line width
##' @param confint Logical. If \code{TRUE} show confidence shadows
##' @param newdata How to adjust
##' @param scale Character string that determines the outcome scale (y-axis). Choose between \code{"risk"} and \code{"odds"}.
##' @param add Logical. If \code{TRUE} add lines to an existing graph
##' @param ... Further arguments passed to \code{plot}. Only if \code{add} is \code{FALSE}.
##' @examples
##' data(Diabetes)
##' Diabetes$hypertension=  1*(Diabetes$bp.1s>140)
##' library(rms)
##' uu <- datadist(Diabetes)
##' options(datadist="uu")
##' fit=lrm(hypertension~rcs(age)+gender+hdl,data=Diabetes)
##' splinePlot.lrm(fit,xvar="age",xvalues=seq(30,50,1))
##' @export 
splinePlot.lrm <- function(object,
                           xvar,
                           xvalues,
                           xlim=range(xvalues),
                           ylim,
                           xlab=xvar,
                           ylab=scale[[1]],
                           col=1,
                           lwd=3,
                           confint=TRUE,
                           newdata=NULL,
                           scale=c("risk","odds"),
                           add=FALSE,...){
    lower=upper=yhat=NULL
    expit <- function (x){exp(x)/(1 + exp(x))}
    input <- list(x=object,xvalues)
    if (!is.null(newdata) && is.list(newdata)){
        input <- c(input,newdata)
    }
    names(input)[[2]] <- xvar
    if (scale[[1]]=="risk") input$fun <- expit
    else{ ## set reference level for odds 
        input$fun <- exp
    }
    pframe <- do.call("Predict",input)
    data.table::setDT(pframe)
    if (missing(ylim)) ylim <- pframe[,c(min(lower),max(upper))]
    if(!add){
        plot(0,0,type="n",ylim=ylim,xlim=xlim,xlab=xlab,ylab=ylab,...)
    }
    pframe[,graphics::lines(xvalues,yhat,lwd=lwd,col=col,type="l",ylim=ylim)]
    if (confint==TRUE){
        pframe[,polygon(x=c(xvalues,rev(xvalues)),y=c(lower,rev(upper)),col=prodlim::dimColor(col),border=NA)]
    }
    pframe
}
######################################################################
### splinePlot.lrm.R ends here
