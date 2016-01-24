##' Expand regression coefficient table
##'
##' This function expands results from "regressionTable" with
##' extralines and columns
##'
##' For factor variables the reference group is shown.
##' For continuous variables the units are shown and
##' for transformed continuous variables also the scale.
##' For all variables the numbers of missing values are added.
##' @title Expand regression coefficient table
##' @param x object resulting from \code{lm}, \code{glm} or \code{coxph}.
##' @param varnames Names of variables
##' @param reference.value Reference value for reference categories
##' @param reference.style Style for showing results for categorical
##' variables. If \code{"extraline"} show an additional line for the
##' reference category.
##' @param factorlevels Levels of the categorical variables.
##' @param scale Scale for some or all of the variables
##' @param nmiss Number of missing values
##' @param intercept Intercept
##' @return a table with regression coefficients
##' @author Thomas Alexander Gerds <tag@@biostat.ku.dk>
##' @export
fixRegressionTable <- function(x,
                               varnames,
                               reference.value,
                               reference.style=NULL,
                               factorlevels,
                               scale=NULL,
                               nmiss,
                               intercept){
    if (missing(nmiss)) nmiss <- NULL
    some.scaled <- sum(scale!="")>0
    ## for some reason logical value variables, ie with levels
    ## TRUE, FALSE do not get xlevels in the output of glm
    loc <- grep("TRUE$",rownames(x),value=TRUE)
    if (length(loc)>0){
        locvars <- lapply(loc,function(l){
            substring(l,1,nchar(l)-4)
        })
        names(locvars) <- locvars
        factorlevels <- c(factorlevels,
                          lapply(locvars,function(l){c("FALSE","TRUE")}))
    }
    factornames <- names(factorlevels)
    ## for some reason ordinal variables get strange labels
    ord <- grep("\\.L$",rownames(x),value=TRUE)
    if (length(ord)>0){
        orderednames <- unlist(strsplit(ord,"\\.L$"))
    }else{orderednames <- ""}
    blocks <- lapply(varnames,function(vn){
        isfactor <- match(vn,factornames,nomatch=0)
        isordered <- match(vn,orderednames,nomatch=0)
        ## the regexp is supposed to catch the term `age' in
        ## age and I(age^2 and interaction(age,sex) and
        ## interaction(sex,age) and fun(age)
        if (isfactor){
            if (isordered){
                vn.regexp <- paste("^",vn,".[LCQ]$","|","",vn,"\\^[0-9]+$",sep="")
            }else{
                levs.regexp <- paste("(",paste(factorlevels[[isfactor]],collapse="|"),")",sep="")
                vn.regexp <- paste("^",vn,levs.regexp,"$","|","I\\(",vn,".*",levs.regexp,"|",vn,"\\)",".*",levs.regexp,sep="")
            }
        } else{
            vn.regexp <- paste("^",vn,"$",sep="")
        }
        parms <- grep(vn.regexp,rownames(x))
        block <- x[parms,,drop=FALSE]
        Scale <- NULL
        Missing <- NULL
        # {{{ discrete variables
        if (isfactor){
            if (reference.style=="inline"){
                Variable <- c(vn,rep("",NROW(block)-1))
                Units <- paste(factorlevels[[isfactor]][-1], "vs", factorlevels[[isfactor]][1])
                if (some.scaled){
                    Scale <- rep("",NROW(block))
                }
                if (!is.null(nmiss)){
                    Missing <- c(nmiss[vn],rep("",NROW(block)-1))
                }
            } else {
                Variable <- c(vn,rep("",NROW(block)))
                Units <- factorlevels[[isfactor]]
                if (some.scaled){
                    Scale <- rep("",NROW(block)+1)
                }
                if (!is.null(nmiss)){
                    Missing <- c(nmiss[vn],rep("",NROW(block)))
                }
                block <- rbind(c(reference.value,rep("",NCOL(block)-1)),block)
            }
        } else{
            # }}}
            # {{{numeric variables
            Variable <- vn
            Units <- ""
            if (!is.null(nmiss)){
                Missing <- nmiss[vn]
            }
            if (some.scaled){
                Scale <- scale[[vn]]
            }
        }
        if (some.scaled){
            do.call("cbind",list(Variable=Variable,
                                 Scale=Scale,
                                 Units=Units,
                                 Missing=as.character(Missing),
                                 block))
        }else{
            do.call("cbind",list(Variable=Variable,
                                 Units=Units,
                                 Missing=as.character(Missing),
                                 block))
        }
        # }}}
    })
    out <- do.call("rbind",blocks)
    out$Variable <- as.character(out$Variable)
    out$Missing <- as.character(out$Missing)
    out$Units <- as.character(out$Units)
    rownames(out) <- 1:NROW(out)
    # {{{ add intercept if it is wanted
    if (intercept!=0 &&
        (found <- match("(Intercept)",rownames(x),nomatch=0))){
        inter <- x[found,,drop=FALSE]
        out <- rbind(unlist(c(Variable="Intercept",
                              Units="",
                              Missing="",
                              inter))[colnames(out)],
                     out)
    }
    rownames(out) <- 1:NROW(out)
    # }}}
    out
}
