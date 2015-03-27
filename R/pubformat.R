### pubformat.R --- 
#----------------------------------------------------------------------
## author: Thomas Alexander Gerds
## created: Feb 21 2015 (10:34) 
## Version: 
## last-updated: Feb 21 2015 (10:46) 
##           By: Thomas Alexander Gerds
##     Update #: 5
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
##' Format numbers according to a specified handler function.
##' Currently supported are sprintf, format and prettyNum.
##'
##' @title Format numbers for publication
##' @param x numeric vector  
##' @param digits number of digits
##' @param nsmall see handler
##' @param handler String specififying the name of the function which should
##' perform the formatting. See \code{sprintf}, \code{format} and \code{prettyNum}.
##' @param ... Passed to handler function if applicable, i.e., not to \code{sprintf}.
##' @return Formatted number
##' @seealso  \code{sprintf}, \code{format}, \code{prettyNum}
##' @examples
##'
##' pubformat(c(0.000143,12.8,1))
##' pubformat(c(0.000143,12.8,1),handler="format")
##' pubformat(c(0.000143,12.8,1),handler="format",trim=TRUE)
##' pubformat(c(0.000143,12.8,1),handler="prettyNum")
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
pubformat <- function(x,digits=2,
                      nsmall=digits,
                      handler="sprintf",...){
    if (handler=="sprintf"){ fmt <- paste0("%1.",digits[[1]],"f")}
    if (handler=="sprintf"){
        sprintf(fmt=fmt,x)
    }else{
        do.call(handler,list(x,digits=digits[[1]],nsmall=nsmall,...))
    }
}
    


#----------------------------------------------------------------------
### pubformat.R ends here
