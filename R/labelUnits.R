##' Label output tables
##'
##' Modify labels and values of variables in summary tables
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
##' ltab <- labelUnits(tab,"chol"="Cholesterol (mg/dL)","<40"="younger than 40")
##' publish(ltab)
##'
##' ## pass labels immediately to utable
##' utable(gender~AgeGroups+chol+waist,data=Diabetes,
##'       "chol"="Cholesterol (mg/dL)","<40"="younger than 40")
##' 
##' ## sometimes useful to state explicitly which variables value
##' ## should be re-labelled
##' utable(gender~AgeGroups+chol+waist,data=Diabetes,
##'       "chol"="Cholesterol (mg/dL)","AgeGroups.<40"="younger than 40")
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
labelUnits <- function(x,...){
    ## stopifnot(match("summary.univariateTable",class(x),nomatch=0)>0)
    x
    units <- prodlim::SmartControl(list(...),
                                   keys=c("units",unique(x$Variable[x$Variable!=""])),
                                   defaults=NULL,
                                   ignore.case=TRUE,
                                   replaceDefaults=TRUE,
                                   verbose=FALSE)
    lunits <- sapply(units,length)
    units <- units[lunits>0]
    ulvar <- grep("Level|Unit",names(x),value=TRUE)
    ## factor specific units
    if (length(units)>0){
        for (i in 1:length(units)){
            uat <- grep(names(units)[i],x$Variable)
            lat <- match(names(units[[i]]),x[[ulvar]][uat:length(x$Variable)],nomatch=FALSE)
            lat <- lat[lat!=0]
            vals <- unlist(units[[i]])
            vals <- vals[lat!=0]
            x[[ulvar]][uat -1 + lat] <- vals
        }
    }
    ## labels for variables
    labels <- list(...)
    if (length(labels)>0){
        keys <- names(labels)
        Flabels <- labels[match(keys,x$Variable,nomatch=0)!=0]
        x$Variable[match(keys,x$Variable,nomatch=0)] <- unlist(Flabels)
        Funits <- labels[match(keys,x[[ulvar]],nomatch=0)!=0]
        for (f in names(Funits)){
            x[[ulvar]][x[[ulvar]]%in%f] <- Funits[[f]]
        }
        ## now flatten lists. otherwise
        ## write.csv will complain 
        x$Variable <- unlist(x$Variable)
        x[[ulvar]] <- unlist(x[[ulvar]])
    }
    x
}
