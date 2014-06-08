##' Summary function for univariate table
##'
##' Collects results of univariate table in a matrix. 
##' @title Preparing univariate tables for publication
##' @param x \code{univariateTable} object as obtained with function \code{univariateTable}.
##' @param missing Decides if number of missing values are shown in table.
##' Defaults to \code{"ifany"}, and can also be set to \code{"always"} or \code{"never"}.
##' 
##' @export
##' @param pvalue.stars If TRUE use \code{symnum} to parse p-values otherwise use \code{format.pval}.
##' @param pvalue.eps Passed to \code{format.pval}.
##' @param pvalue.digits Passed to \code{format.pval}.
##' @param ... 
##' @return Summary table 
##' @author Thomas A. Gerds
summary.univariateTable <- function(x,
                                    missing=c("ifany","always","never"),
                                    n="inNames",
                                    pvalue.stars=FALSE,
                                    pvalue.eps=0.0001,
                                    pvalue.digits=4,
                                    ...){
    # {{{missing and n
    missing <- match.arg(missing,c("ifany","always","never"),several.ok=FALSE)
    # }}}
    # {{{ pvalues
    if (!is.null(x$p.values)){
        if (pvalue.stars==TRUE)
            px <- symnum(x$p.values,corr = FALSE,na = FALSE,cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),symbols = c("***", "**", "*", ".", " "))
        else
            px <- format.pval(x$p.values,eps=pvalue.eps,digits=pvalue.digits)
        names(px) <- names(x$p.values)
    }
    # }}}
    # {{{ order the table according to formula
    if (is.null(x$groups)){
        XX <- all.vars(x$formula)
    }
    else{
        XX <- all.vars(x$formula)[-1]
    }
    order <- match(XX,names(x$summary.groups))
    ordered.summary <- x$summary.groups[order]
    # }}}
    XXtab <- NULL
    # {{{ loop across table elements
    for (s in names(ordered.summary)){
        if (!is.null(x$groups)){
            sum <- as.matrix(ordered.summary[[s]])
            sum <- cbind(sum,Total=x$summary.totals[[s]])
        }
        else{
            sum <- data.frame(Total=x$summary.totals[[s]],stringsAsFactors = FALSE)
        }
        if (missing!="never" && (missing=="always" || any(x$missing$totals[[s]]>0))){
            if (is.null(x$groups)){
                miss <- x$missing$totals[[s]]
            }
            else{
                miss <- c(unlist(x$missing$group[[s]]),x$missing$totals[[s]])
            }
        }
        else{
            miss <- NULL
        }
        sum <- rbind(sum,miss)
        if (x$vartype[[s]]=="factor"){
            lev <- x$xlevels[[s]]
        }
        else{
            if (x$vartype[[s]]=="Q") 
                lev <- gsub("\\(x\\)","",x$Q.format)
            else
                lev <- gsub("\\(x\\)","",x$summary.format)
        }
        if (!is.null(miss)) lev <- c(lev,"missing")
        if (!is.null(x$groups)){
            p <- px[[s]]
            if (NROW(sum)>2 && NROW(p)==(NROW(sum)-1)){
                sum <- cbind(sum,rbind(rep("",NROW(sum)-1),p=px[[s]]))
                colnames(sum)[NCOL(sum)] <- "p"
            }
            else{
                if (is.null(miss)){
                    p <- c(rep("",NROW(sum)-1),px[[s]])
                }
                else{
                    p <- c(rep("",NROW(sum)-2),px[[s]],"")
                }
                sum <- cbind(sum,p)
            }
        }
        ## fac <- c(s,rep("",NROW(sum)-1))
        fac <- c(s,rep("",length(lev)-1))
        sum <- cbind(unlist(fac),lev,sum)
        ## if (NROW(sum)>2)
        sumXX <- data.frame(sum,stringsAsFactors=FALSE,row.names=1:NROW(sum))
        rownames(sumXX) <- NULL
        XXtab <- rbind(XXtab,sumXX)
    }
    ## rownames(XXtab) <- 1:NROW(XXtab)
    # }}}
    # {{{ column names and n

    if (!missing(n)){
        if (n=="inNames"){
            x$groups <- paste(x$groups," (n=",x$n.groups[-length(x$n.groups)],")",sep="")
        }
        else{
            XXtab <- rbind(c("n","",x$n.groups,""),XXtab)
        }
    }
    if (is.null(x$groups)){
        colnames(XXtab) <- c("Variable","Levels","Value")
        XXtab$Variable <- as.character(XXtab$Variable)
        XXtab$Levels <- as.character(XXtab$Levels)
    }
    else{
        if (!missing(n) && (n=="inNames")){
            colnames(XXtab) <- c("Variable","Level",x$groups,paste("Total"," (n=",x$n.groups[length(x$n.groups)],")",sep=""),"p-value")
        }
        else{
            colnames(XXtab) <- c("Variable","Level",x$groups,"Total","p-value")
        }
    }
    # }}}
    # {{{ labels & units
    class(XXtab) <- c("summary.univariateTable","data.frame")
    XXtab <- labelUnits(XXtab,...)
    # }}}
    rownames(XXtab) <- NULL
    XXtab
}
