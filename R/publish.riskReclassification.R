### publish.riskReclassification.R --- 
#----------------------------------------------------------------------
## author: Thomas Alexander Gerds
## created: Dec 10 2015 (10:06) 
## Version: 
## last-updated: Oct 22 2017 (12:55) 
##           By: Thomas Alexander Gerds
##     Update #: 8
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
publish.riskReclassification <- function(x,percent=TRUE,digits=ifelse(percent,1,2),...){
    cat("Observed overall re-classification table:\n\n")
    dnames <- dimnames(x$reclassification)
    cat(names(dnames)[1]," versus ", names(dnames)[2],"\n")
    publish(x$reclassification,...)
    cat("\nExpected re-classification probabilities (%) among subjects with event until time ",x$time,"\n\n",sep="")
    fmt <- paste0("%1.", digits[[1]], "f")
    dim <- dim(x$reclassification)
    if (percent==TRUE){
        rlist <- lapply(x$event.reclassification,function(x){
                            matrix(sprintf(fmt=fmt,100*c(x)),nrow=dim[1],ncol=dim[2],dimnames=dnames)
                        })
    }else{
         rlist <- lapply(x$event.reclassification,function(x){
                             matrix(sprintf(fmt=fmt,c(x)),nrow=dim[1],ncol=dim[2],dimnames=dnames)
                         })
     }
    if (x$model=="competing.risks"){
        for (x in 1:(length(rlist)-1)){
            cat("\n",names(rlist)[x],":\n",sep="")
            publish(rlist[[x]],quote=FALSE,...)
        }
    } else{
          cat("\n",names(rlist)[1],":\n",sep="")
          publish(rlist[[1]],quote=FALSE,...)
      }
    cat("\nExpected re-classification probabilities (%) among subjects event-free until time ",x$time,"\n\n",sep="")
    cat("\n",names(rlist)[length(rlist)],":\n",sep="")
    publish(rlist[[length(rlist)]],quote=FALSE,...)
    ## print.listof(rlist[length(rlist)],quote=FALSE)
}
#----------------------------------------------------------------------
### publish.riskReclassification.R ends here
