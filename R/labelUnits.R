labelUnits <- function(x,...){
    units <- SmartControl(list(...),
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
