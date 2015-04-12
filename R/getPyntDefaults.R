### getPyntDefaults.R --- 
#----------------------------------------------------------------------
## author: Thomas Alexander Gerds
## created: Feb 26 2015 (06:54) 
## Version: 
## last-updated: Feb 26 2015 (07:20) 
##           By: Thomas Alexander Gerds
##     Update #: 10
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
getPyntDefaults <- function(call,names){
    call <- as.list(call)
    pub.args <- call[match(names(names),names(call),nomatch=FALSE)]
    pynt <- lapply(names(names),function(n){
        if (length(pa <- pub.args[[n]])>0)
            eval(pa)
        else
            names[[n]]
    })
    names(pynt) <- names(names)
    pynt
}
#----------------------------------------------------------------------
### getPyntDefaults.R ends here
