##' Expand tables showing regression coefficients
##'
##' For factors a reference line is added. Shown are numbers of missing values, 
##' units and for log transformed variables the scale.
##' @title Expand regression coefficients
##' @param x
##' @param varnames
##' @param reference.value
##' @param reference.style
##' @param factorlevels
##' @param scale
##' @param nmiss
##' @param intercept
##' @return 
##' @author Thomas Alexander Gerds
fixRegressionTable <- function(x,
                               varnames,
                               reference.value,
                               reference.style=NULL,
                               factorlevels,
                               scale,
                               nmiss,
                               intercept){
    if (missing(nmiss)) nmiss <- NULL
    some.scaled <- sum(scale=="")>0
    ## for some reason logical value variables, ie with levels
    ## TRUE, FALSE do not get xlevels in the output of glm
    loc <- grep("TRUE$",rownames(x),value=TRUE)
    if (length(loc)>0){
        locvars <- lapply(loc,function(l){
            substring(l,1,nchar(l)-4)
        })
        names(locvars) <- locvars
        factorlevels <- c(factorlevels,lapply(locvars,function(l){c("FALSE","TRUE")}))
    }
    factornames <- names(factorlevels)
    blocks <- lapply(varnames,function(vn){
        isfactor <- match(vn,factornames,nomatch=0)
        ## the regexp is supposed to catch the term `age' in
        ## age and I(age^2 and interaction(age,sex) and
        ## interaction(sex,age) and fun(age)
        if (isfactor){
            levs.regexp <- paste("(",paste(factorlevels[[isfactor]],collapse="|"),")",sep="")
            vn.regexp <- paste("^",vn,levs.regexp,"$","|","I\\(",vn,".*",levs.regexp,"|",vn,"\\)",".*",levs.regexp,sep="")
        } else{
            vn.regexp <- paste("^",vn,"$","|","\\(",vn,"|",vn,"\\)",sep="")
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
                block <- rbind(c(reference.value,rep("",NCOL(block)-1)), block)
            }
        } else{ ## numeric variables
            Variable <- vn
            Units <- ""
            if (!is.null(nmiss)){
                Missing <- nmiss[vn]
            }
            if (some.scaled){
                Scale <- scale[[vn]]
            }
        }
        do.call("cbind",list(Variable=Variable,Scale=Scale,Units=Units,Missing=Missing,block))
    })
    out <- do.call("rbind",blocks)
    out$Variable <- as.character(out$Variable)
    out$Units <- as.character(out$Units)
    rownames(out) <- 1:NROW(out)
    # {{{ add intercept if it is wanted
    if (intercept!=0 && 
        (found <- match("(Intercept)",rownames(x),nomatch=0))){
        inter <- x[found,,drop=FALSE]
        out <- rbind(c(Variable="Intercept",Units="",Missing="",inter)[colnames(out)],
                     out)
    }
    rownames(out) <- 1:NROW(out)
    # }}}
    out
}
