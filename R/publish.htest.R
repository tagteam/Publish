##' @export
publish.htest <- function(object,title,digits=3,peps=0.0001,pdigits=4,ciformat="[l;u]",...){
    printmethod=object$method
    printmethod[grep("Wilcoxon rank sum test",printmethod)]="Wilcoxon rank sum test"
    printmethod[grep("Wilcoxon signed rank test",printmethod)]="Wilcoxon signed rank test"
    printmethod[grep("Two Sample t-test",printmethod)]="Two Sample t-test"
    if (!is.null(object$conf.int)){
        if (printmethod=="Exact binomial test"){
            cistring=paste(" (CI-",
                100*attr(object$conf.int,"conf.level"),
                "% = ",
                formatCI(sep="",format=ciformat,lower=object$conf.int[[1]],upper=object$conf.int[[2]],digits=digits),
                ").",sep="")
        }else{
            cistring=paste(" (CI-",
                100*attr(object$conf.int,"conf.level"),
                "% = ",
                formatCI(sep="",format=ciformat,lower=object$conf.int[[1]],upper=object$conf.int[[2]],digits=digits),
                "; ",
                "p-value = ",
                format.pval(object$p.value,digits=pdigits,eps=peps),
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
                                  format(object$estimate,
                                         digits=digits,
                                         nsmall=digits),
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
                                  format(diff(object$estimate),
                                         digits=digits,
                                         nsmall=digits),
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
                                      format.pval(object$p.value,digits=pdigits,eps=peps),
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
                                      format(object$estimate,digits=digits,nsmall=digits),
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
                                  format(object$estimate,digits=digits,nsmall=digits),
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
                                      format.pval(object$p.value,digits=pdigits,eps=peps),
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
                                      format(object$estimate,digits=digits,nsmall=digits),
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
    
    
    
