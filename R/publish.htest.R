##' Pretty printing of test results.
##' 
##' @title Pretty printing of test results.
##' @export
##' @param object Result of \code{t.test} or \code{wilcox.test}
##' @param title Decoration also used to name output
##' @param ... Used to transport arguments \code{ci.arg} and \code{pvalue.arg} to subroutines \code{format.pval} and \code{formatCI}. See also \code{prodlim::SmartControl}.
##' @author Thomas A. Gerds <tag@@biostat.ku.dk> 
##' @examples
##' data(Diabetes)
##' publish(t.test(bp.2s~gender,data=Diabetes))
##' publish(wilcox.test(bp.2s~gender,data=Diabetes))
##' publish(with(Diabetes,t.test(bp.2s,bp.1s,paired=TRUE)))
##' publish(with(Diabetes,wilcox.test(bp.2s,bp.1s,paired=TRUE)))
##' 
publish.htest <- function(object,
                          title,
                          ...){
    pynt <- getPyntDefaults(list(...),names=list("digits"=c(2,3),"handler"="sprintf",nsmall=NULL))
    digits <- pynt$digits
    if (length(digits)==1) digits <- rep(digits,2)
    handler <- pynt$handler
    if (length(pynt$nsmall)>0) nsmall <- pynt$nsmall else nsmall <- pynt$digits
    Lower <- object$conf.int[[1]]
    Upper <- object$conf.int[[2]]
    ci.defaults <- list(format="[l;u]",
                        digits=digits[[1]],
                        nsmall=digits[[1]],
                        degenerated="asis")
    pvalue.defaults <- list(digits=digits[[2]],
                            eps=10^{-digits[[2]]},
                            stars=FALSE)
    smartF <- prodlim::SmartControl(call=list(...),
                                    keys=c("ci","pvalue"),
                                    ignore=c("x","print","handler","digits","nsmall"),
                                    defaults=list("ci"=ci.defaults,"pvalue"=pvalue.defaults),
                                    forced=list("ci"=list(lower=Lower,upper=Upper,handler=handler,digits=digits[[1]],nsmall=nsmall[[1]]),
                                        "pvalue"=list(object$p.value)),
                                    verbose=FALSE)
    printmethod=object$method
    printmethod[grep("Wilcoxon rank sum test",printmethod)]="Wilcoxon rank sum test"
    printmethod[grep("Wilcoxon signed rank test",printmethod)]="Wilcoxon signed rank test"
    printmethod[grep("Two Sample t-test",printmethod)]="Two Sample t-test"
    if (!is.null(object$conf.int)){
        if (printmethod=="Exact binomial test"){
            cistring=paste(" (CI-",
                100*attr(object$conf.int,"conf.level"),
                "% = ",
                do.call("formatCI",smartF$ci),
                ").",sep="")
        }else{
            cistring=paste(" (CI-",
                100*attr(object$conf.int,"conf.level"),
                "% = ",
                do.call("formatCI",smartF$ci),
                "; ",
                "p-value = ",
                do.call("format.pval",smartF$pvalue),
                ").",sep="")
        }
    } else{
        cistring=""
    }
    switch(printmethod,
           "Exact binomial test"={
               outstring <- paste("The ",
                                  object$method,
                                  " to estimate the ",
                                  names(object$null.value),
                                  " based on ", object$statistic,
                                  " events ",
                                  " in ", object$parameter,
                                  " trials yields a probability estimate of ",
                                  pubformat(object$estimate,handler=handler, digits=digits[[1]], nsmall=nsmall[[1]]),
                                  cistring,
                                  sep="")
           },
           "Two Sample t-test"={
               outstring <- paste("The ",
                                  object$method,
                                  " to compare the ",
                                  names(object$null.value),
                                  " for ",
                                  object$data.name,
                                  " yields a mean difference of ",
                                  pubformat(diff(object$estimate),handler=handler, digits=digits[[1]], nsmall=nsmall[[1]]),
                                  cistring,
                                  sep="")
           },
           "Wilcoxon rank sum test"={
               if (is.null(object$conf.int))
                   outstring <- paste("The ",
                                      object$method,
                                      " to compare the ",
                                      names(object$null.value),
                                      " for ",
                                      object$data.name,
                                      " yields a p-value of ",
                                      do.call("format.pval",smartF$pvalue),
                                      ".",
                                      sep="")
               else
                   outstring <- paste("The ",
                                      object$method,
                                      " to compare the ",
                                      names(object$null.value),
                                      " for ",
                                      object$data.name,
                                      " yields a ",
                                      names(object$estimate),
                                      " of ",
                                      pubformat(object$estimate,handler=handler, digits=digits[[1]], nsmall=nsmall[[1]]),
                                      cistring,
                                      sep="")
           },
           "Paired t-test"={
               outstring <- paste("The ",
                                  object$method,
                                  " to compare the ",
                                  names(object$null.value),
                                  " for ",
                                  object$data.name,
                                  " yields a mean of the differences of ",
                                  pubformat(object$estimate,handler=handler, digits=digits[[1]], nsmall=nsmall[[1]]),
                                  cistring,
                                  sep="")
           },
           "Wilcoxon signed rank test"={
               if (is.null(object$conf.int))
                   outstring <- paste("The ",
                                      object$method,
                                      " to compare the ",
                                      names(object$null.value),
                                      " for ",
                                      object$data.name,
                                      " yields a p-value of ",
                                      do.call("format.pval",smartF$pvalue),
                                      ".",
                                      sep="")
               else
                   outstring <- paste("The ",
                                      object$method,
                                      " to compare the ",
                                      names(object$null.value),
                                      " for ",
                                      object$data.name,
                                      " yields a ",
                                      names(object$estimate),
                                      " of ",
                                      pubformat(object$estimate,handler=handler, digits=digits[[1]], nsmall=nsmall[[1]]),                                      
                                      cistring,
                                      sep="")
           })
    outstring=gsub('[[:space:]]+',' ',gsub('[[:space:]]$','',outstring))
    if (missing(title))
        cat("\n",outstring,"\n")
    else{
        names(outstring) <- title
        print(outstring,quote=F)
    }
}
    
    
    
