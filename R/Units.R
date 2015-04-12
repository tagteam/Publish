### Units.R --- 
#----------------------------------------------------------------------
## author: Thomas Alexander Gerds
## created: Apr  9 2015 (10:35) 
## Version: 
## last-updated: Apr  9 2015 (10:54) 
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
##' Add variable units to data.frame (or data.table).
##'
##' If the object has units existing units are replaced by given units.
##' @title Add units to data set
##' @param object A data.frame or data.table
##' @param units Named list of units. Names are variable names. If omitted, show existing units.
##' @return
##' The object augmented with attribute \code{"units"}
##' @examples
##' data(Diabetes)
##' Diabetes <- Units(Diabetes,list(BMI="kg/m^2"))
##' Units(Diabetes)
##' Diabetes <- Units(Diabetes,list(bp.1s="mm Hg",bp.2s="mm Hg"))
##' Units(Diabetes)
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
Units <- function(object,units){
    stopifnot("data.frame" %in% class(object))
    if (missing(units)){
        return(attr(object,"units"))
    }
    else{
        old.units <- attr(object,"units")
        if (is.null(old.units))
            attr(object,"units") <- units
        else{
            new.units <- c(units,old.units)
            new.units <- new.units[!duplicated(names(new.units))]
            attr(object,"units") <- new.units
        }
    }
    object
}

#----------------------------------------------------------------------
### Units.R ends here
