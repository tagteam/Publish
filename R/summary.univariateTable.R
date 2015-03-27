##' Summary function for univariate table
##'
##' Collects results of univariate table in a matrix. 
##' @title Preparing univariate tables for publication
##' @param object \code{univariateTable} object as obtained with
##' function \code{univariateTable}.
##' @param missing Decides if number of missing values are shown in table.
##' Defaults to \code{"ifany"}, and can also be set to \code{"always"} or \code{"never"}.
##' 
##' @param n If not missing, show the number of subjects in each
##' column. If equal to \code{"inNames"}, show the numbers in
##' parentheses in the column names.
##' @param pvalue.stars If TRUE use \code{symnum} to parse p-values
##' otherwise use \code{format.pval}.
##' @param pvalue.eps Passed to \code{format.pval}.
##' @param pvalue.digits Passed to \code{format.pval}.
##' @param ... passed on to labelUnits
##' @export
##' @return Summary table 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
##' @examples
##' data(Diabetes)
##' u <- univariateTable(gender~age+location+Q(BMI)+height+weight,
##'                 data=Diabetes)
##' summary(u)
##' summary(u,n=NULL)
##' summary(u,pvalue.digits=2,"age"="Age (years)","height"="Body height (cm)")
##' 
##' 
summary.univariateTable <- function(object,
                                    missing=c("ifany","always","never"),
                                    n="inNames",
                                    pvalue.stars=FALSE,
                                    pvalue.digits=4,
                                    ...){
    # {{{missing and n
    missing <- match.arg(missing,c("ifany","always","never"),several.ok=FALSE)
    # }}}
    # {{{ pvalues
    if (!is.null(object$p.values)){
        if (pvalue.stars==TRUE)
            px <- symnum(object$p.values,corr = FALSE,na = FALSE,cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),symbols = c("***", "**", "*", ".", " "))
        else
            px <- format.pval(object$p.values,eps=10^{-pvalue.digits},digits=pvalue.digits)
        names(px) <- names(object$p.values)
    }
    # }}}
    # {{{ order the table according to formula
    if (is.null(object$groups)){
        XX <- all.vars(object$formula)
    }
    else{
        XX <- all.vars(object$formula)[-1]
    }
    order <- match(XX,names(object$summary.groups))
    ordered.summary <- object$summary.groups[order]
    # }}}
    XXtab <- NULL
    # {{{ loop across table elements
    for (s in names(ordered.summary)){
        if (!is.null(object$groups)){
            sum <- as.matrix(ordered.summary[[s]])
            sum <- cbind(sum,Total=object$summary.totals[[s]])
        }
        else{
            sum <- data.frame(Total=object$summary.totals[[s]],stringsAsFactors = FALSE)
        }
        if (missing!="never" && (missing=="always" || any(object$missing$totals[[s]]>0))){
            if (is.null(object$groups)){
                miss <- object$missing$totals[[s]]
            }
            else{
                miss <- c(unlist(object$missing$group[[s]]),object$missing$totals[[s]])
            }
        }
        else{
            miss <- NULL
        }
        sum <- rbind(sum,miss)
        if (object$vartype[[s]]=="factor"){
            lev <- object$xlevels[[s]]
        }
        else{
            if (object$vartype[[s]]=="Q") 
                lev <- gsub("\\(x\\)","",object$Q.format)
            else
                lev <- gsub("\\(x\\)","",object$summary.format)
        }
        if (!is.null(miss)) lev <- c(lev,"missing")
        if (!is.null(object$groups)){
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

    ## if (!missing(n)){
    if (length(n)>0)
        if (n=="inNames"){
            object$groups <- paste(object$groups," (n=",object$n.groups[-length(object$n.groups)],")",sep="")
        }
        else{
            XXtab <- rbind(c("n","",object$n.groups,""),XXtab)
        }
    ## }
    if (is.null(object$groups)){
        colnames(XXtab) <- c("Variable","Levels","Value")
        XXtab$Variable <- as.character(XXtab$Variable)
        XXtab$Levels <- as.character(XXtab$Levels)
    }
    else{
        if (length(n)>0 && (n=="inNames")){
            colnames(XXtab) <- c("Variable","Level",object$groups,paste("Total"," (n=",object$n.groups[length(object$n.groups)],")",sep=""),"p-value")
        }
        else{
            colnames(XXtab) <- c("Variable","Level",object$groups,"Total","p-value")
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
