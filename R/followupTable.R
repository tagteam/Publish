### followupTable.R --- 
#----------------------------------------------------------------------
## author: Thomas Alexander Gerds
## created: Nov 28 2015 (08:23) 
## Version: 
## last-updated: Oct 22 2017 (12:55) 
##           By: Thomas Alexander Gerds
##     Update #: 50
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
##' Summarize baseline variables in groups defined by outcome
##' at a given followup time point
##'
##' If \code{compare.groups!=FALSE}, p-values are obtained from stopped Cox regression, i.e., all events are censored at follow-up time.
##' A univariate Cox regression model is fitted to assess the effect of each variable on the right hand side of the formula on the event hazard and shown is the p-value of \code{anova(fit)}, see \code{\link{anova.coxph}}.
##  With competing risks the same is done for the hazard of being event-free (combined end-point analysis).
##' @title Summary tables for a given followup time point.
##' @param formula Formula A formula whose left hand side is a
##' \code{Hist} object. In some special cases it can also be a
##' \code{Surv} response object.  The right hand side is as in
##' \code{\link{utable}}.
##' @param data A data.frame in which all the variables of
##' \code{formula} can be interpreted.
##' @param followup.time Time point at which to evaluate outcome
##' status.
##' @param compare.groups Method for comparing groups. 
##' @param ... Passed to \code{utable}. All arguments of \code{utable}
##' can be controlled in this way except for \code{compare.groups}
##' which is set to \code{"Cox"}. See details.
##' @return
##' Summary table.
##' @seealso univariateTable
##' @examples
##' library(survival)
##' data(pbc)
##' pbc$edema <- factor(pbc$edema,levels=c("0","0.5","1"),labels=c("0","0.5","1"))
##' pbc$sex <- factor(pbc$sex,levels=c("m","f"),labels=c("m","f"))
##' followupTable(Hist(time,status)~age+edema+sex,data=pbc,followup.time=1000)
##' 
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
followupTable <- function(formula,data,followup.time,compare.groups,...){
    event.history <- prodlim::EventHistory.frame(update(formula,".~1"),
                                                 data=data,
                                                 check.formula=TRUE,
                                                 specials=NULL)$event.history
    # {{{ Fix for those who use `Surv' instead of `Hist' 
    if (match("Surv",class(event.history),nomatch=0)!=0){
        attr(event.history,"model") <- "survival"
        attr(event.history,"cens.type") <- "rightCensored"
        attr(event.history,"entry.type") <- ifelse(ncol(event.history)==2,"","leftTruncated")
        if (attr(event.history,"entry.type")=="leftTruncated")
            colnames(event.history) <- c("entry","time","status")
    }
    # }}}
    if (length(attr(event.history,"entry.type"))>1) stop("Cannot handle delayed entry.")
    if (missing(followup.time)) 
        followup.time <- NULL
    else{
        time <- event.history[,"time",drop=TRUE]
    }
    model <- attr(event.history,"model")
    if (model=="survival"){
        status <- event.history[,"status",drop=TRUE]
        status <- as.character(factor(status,levels=c(0,1),labels=c("Lost","Event")))
        status[event.history[,"time"]>followup.time] <- "Event-free"
        ## ehs <- prodlim::stopTime(event.history)
    }else{ 
        if (model!="competing.risks") stop("Can only handle survival and competing risks outcome.")
        ## status <- getEvent(event.history,mode="numeric")
        status <- getEvent(event.history,mode="factor")
        ## status <- getEvent(event.history,mode="character")
        slevs <- unique(c(levels(status),"Event-free"))
        levels(status) <- slevs
        ## status[event.history[,"time"]>followup.time] <- length(attr(event.history,"states"))+1
        status[event.history[,"time"]>followup.time] <- "Event-free"
    }
    if (length(followup.time)==0) stop("Need a followup time.")
    ## FIXME: need a time otherwise all are unknown.
    uformula <- update(formula,"fstatus~.")
    ## groupname <- "status"
    data$fstatus <- status
    if (missing(compare.groups)){
        dots <- match.call(expand.dots=TRUE)
        compare.groups <- dots$compare.groups
        if (length(compare.groups)==0)
            compare.groups <- "Cox"
        else
            compare.groups <- NULL
    }
    if (length(compare.groups)>0 && compare.groups!=FALSE){
        outcome <- unclass(prodlim::stopTime(event.history,stop.time=followup.time))
        ## for now: effect on event-free survival 
        if (model=="competing.risks"){
            outcome[,"status"] <- outcome[,"status"]!=0
        }
    } else{
        compare.groups <- FALSE
        outcome <- NULL
    }
    utable(formula=uformula,
           data=data,
           outcome=outcome,
           compare.groups=compare.groups,
           ...)
}


#----------------------------------------------------------------------
### followupTable.R ends here
