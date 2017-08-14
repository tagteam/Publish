### publish.Score.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Jun 10 2017 (17:47) 
## Version: 
## Last-Updated: Jun 10 2017 (17:56) 
##           By: Thomas Alexander Gerds
##     Update #: 3
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
publish.Score <- function(object,digits=3,level=3,...){
    for (m in object$metrics){
        org(paste0("Metric ",m,":\n"),level=level)
        org("Assessment of models",level=level+1)
        publish(object[[m]]$score,digits=digits, ...)
        if (!is.null(object[[m]]$contrasts)){
            org("Comparison of models",level=level+1)
            publish(object[[m]]$contrasts,digits=digits, ...)
        }
    }
}


######################################################################
### publish.Score.R ends here
