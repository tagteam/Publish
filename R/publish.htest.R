publish.htest <- function(x,title,digits=3,peps=0.0001,pdigits=4,...){
  printmethod=x$method
  printmethod[grep("Wilcoxon rank sum test",printmethod)]="Wilcoxon rank sum test"
  printmethod[grep("Wilcoxon signed rank test",printmethod)]="Wilcoxon signed rank test"
  printmethod[grep("Two Sample t-test",printmethod)]="Two Sample t-test"
  if (!is.null(x$conf.int)) cistring=paste(" (CI-",
                    100*attr(x$conf.int,"conf.level"),
                    "% = ",
                    format.ci(sep="",style=2,lower=x$conf.int[[1]],upper=x$conf.int[[2]],digits=digits),
                    "; ",
                    "p-value = ",
                              format.pval(x$p.value,digits=pdigits,eps=peps),
                    ").",sep="")
  else cistring=""
  switch(printmethod,
         "Two Sample t-test"={
           outstring <- paste("The ",x$method," to compare the ",names(x$null.value)," for ",x$data.name," yields a mean difference of ",-round(diff(x$estimate),digits),cistring,sep="")
         },
         "Wilcoxon rank sum test"={
           if (is.null(x$conf.int))
             outstring <- paste("The ",x$method," to compare the ",names(x$null.value)," for ",x$data.name," yields a p-value of ",format.pval(x$p.value,digits=pdigits,eps=peps),".",sep="")
           else
             outstring <- paste("The ",x$method," to compare the ",names(x$null.value)," for ",x$data.name," yields a ",names(x$estimate)," of ",round(x$estimate,digits),cistring,sep="")
         },
         "Paired t-test"={
           outstring <- paste("The ",x$method," to compare the ",names(x$null.value)," for ",x$data.name," yields a mean of the differences of ",round(x$estimate,digits),cistring,sep="")
         },
         "Wilcoxon signed rank test"={
           if (is.null(x$conf.int))
             outstring <- paste("The ",x$method," to compare the ",names(x$null.value)," for ",x$data.name," yields a p-value of ",format.pval(x$p.value,digits=pdigits,eps=peps),".",sep="")
           else
             outstring <- paste("The ",x$method," to compare the ",names(x$null.value)," for ",x$data.name," yields a ",names(x$estimate)," of ",round(x$estimate,digits),cistring,sep="")
         })
  outstring=gsub('[[:space:]]+',' ',gsub('[[:space:]]$','',outstring))
  if (missing(title))
    cat("\n",outstring,"\n")
  else{
    names(outstring) <- title
    print(outstring,quote=F)
  }
}
    
    
    
