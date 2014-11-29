##' Label output tables
##'
##' Change printed variable labels and values  
##' @title labelUnits
##' @param x A matrix obtained with \code{univariateTable}.
##' @param ... not used
##' @return The re-labeled matrix
##' @seealso univariateTable
##' @examples
##'
##' data(Diabetes)
##' tab <- summary(univariateTable(gender~AgeGroups+chol+waist,data=Diabetes))
##' publish(tab)
##' ltab <- labelUnits(tab,"chol"="Cholesterol (mg/dL)","age<40"="younger than 40")
##' publish(ltab)
##' 
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
labelUnits <- function(x,...){
    ## stopifnot(match("summary.univariateTable",class(x),nomatch=0)>0)
    units <- prodlim::SmartControl(list(...),
                          keys=c("units",unique(x$Variable[x$Variable!=""])),
                          defaults=NULL,
                          ignore.case=TRUE,
                          replaceDefaults=TRUE,
                          verbose=FALSE)
    lunits <- sapply(units,length)
    units <- units[lunits>0]
    ## factor specific units
    if (length(units)>0){
        for (i in 1:length(units)){
            if (tolower(names(units)[i])=="units"){
                for (v in names(units[[i]])){
                    x$Units[match(v,x$Variable,nomatch=0)] <- units[[i]][v]
                }
            }else{
                uat <- grep(names(units)[i],x$Variable)
                lat <- match(names(units[[i]]),x$Units[uat:length(x$Variable)],nomatch=FALSE)
                lat <- lat[lat!=0]
                vals <- unlist(units[[i]])
                vals <- vals[lat!=0]
                x$Units[uat -1 + lat] <- vals
            }
        }
    }
    ## labels
    labels <- list(...)
    if (length(labels)>0){
        keys <- names(labels)
        Flabels <- labels[match(keys,x$Variable,nomatch=0)!=0]
        x$Variable[match(keys,x$Variable,nomatch=0)] <- Flabels
        Funits <- labels[match(keys,x$Units,nomatch=0)!=0]
        for (f in names(Funits)){
            x$Units[x$Units%in%f] <- Funits[[f]]
        }
    }
    x
}
