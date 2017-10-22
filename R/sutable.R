### sutable.R --- 
#----------------------------------------------------------------------
## author: Thomas Alexander Gerds
## created: Nov 28 2015 (08:40) 
## Version: 
## last-updated: Oct 22 2017 (12:57) 
##           By: Thomas Alexander Gerds
##     Update #: 7
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
## the sutable first calls utable and then summary
##' First apply univariateTable then call summary.
##'
##' @title Fast summary of a univariate table
##' @param ... Unnamed arguments and are passed to \code{univariateTable} as well as named arguments
##' that match \code{univariateTable}'s arguments, other arguments
##' are passed to \code{summary.univariateTable}
##' @return Summary table
##' @seealso summary.univariateTable univariateTable 
##' @examples
##' data(Diabetes)
##' sutable(gender~age+location+Q(BMI)+height+weight,data=Diabetes,BMI="Body mass index (kg/m^2)")
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
sutable <- function(...){
    args <- list(...)
    unames <- c("formula","data","summary.format","Q.format","freq.format","column.percent","digits","strataIsOutcome","short.groupnames","na.rm")
    ## no name arguments go into utable
    uargs <- args[names(args)==""]
    args <- args[names(args)!=""]
    test.args <- match(names(args),unames,nomatch=0)
    sargs <- args[test.args==0]
    uargs <- c(uargs,args[test.args!=0])
    do.call(summary,c(list(object=do.call(univariateTable,uargs)),sargs))
}

#----------------------------------------------------------------------
### sutable.R ends here
