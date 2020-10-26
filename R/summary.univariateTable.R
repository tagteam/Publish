##' Summary function for univariate table
##'
##' Collects results of univariate table in a matrix. 
##' @title Preparing univariate tables for publication
##' @param object \code{univariateTable} object as obtained with
##' function \code{univariateTable}.
##' @param n If not missing, show the number of subjects in each
##' column. If equal to \code{"inNames"}, show the numbers in
##' parentheses in the column names.  If missing the value
##' \code{object$n} is used.
##' @param drop.reference Logical or character (vector). Decide if line with reference
##' level should be suppressed for factors. If \code{TRUE} or \code{"all"}
##' suppress for all categorical factors. If \code{'binary'} suppress only for binary variables.
##' Can be character vector in which case reference lines are suppressed for variables
##' that are included in the vector.
##' @param pvalue.stars If TRUE use \code{symnum} to parse p-values
##' otherwise use \code{format.pval}.
##' @param pvalue.digits Passed to \code{format.pval}.
##' @param show.missing Decides if number of missing values are shown in table.
##' Defaults to \code{"ifany"}, and can also be set to \code{"always"} or \code{"never"}.
##' @param show.pvalues Logical. If set to \code{FALSE} the column
##' \code{p-values} is removed. If missing the value
##' \code{object$compare.groups[[1]]==TRUE} is used.
##' @param show.totals Logical. If set to \code{FALSE} the column
##' \code{Totals} is removed. If missing the value
##' \code{object$show.totals} is used.
##' @param ... passed on to \code{labelUnits}. This overwrites labels
##' stored in \code{object$labels}
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
##' u2 <- univariateTable(location~age+AgeGroups+gender+height+weight,
##'                 data=Diabetes)
##' summary(u2)
##' summary(u2,drop.reference=TRUE)
##' ## same but more flexible
##' summary(u2,drop.reference=c("binary"))
##' ## same but even more flexible
##' summary(u2,drop.reference=c("gender"))
##' 
##' 
summary.univariateTable <- function(object,
                                    n="inNames",
                                    drop.reference=FALSE,
                                    pvalue.stars=FALSE,
                                    pvalue.digits=4,
                                    show.missing=c("ifany","always","never"),
                                    show.pvalues,
                                    show.totals,
                                    ...){
    if (missing(show.totals))
        show.totals <- object$show.totals
    if (missing(n))
        n <- object$n
    if (missing(show.pvalues))
        show.pvalues <- object$compare.groups[[1]]==TRUE
    # {{{missing and n
    if (!missing(show.missing))
        if (is.logical(show.missing) || is.numeric(show.missing))
            if (show.missing==1L) show.missing <- "always"
            else show.missing <- "never"
    show.missing <- match.arg(show.missing,c("ifany","always","never"),several.ok=FALSE)
    # }}}
    # {{{ pvalues
    if (show.pvalues && !is.null(object$p.values)){
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
            if (show.totals)
                sum <- cbind(sum,Total=object$summary.totals[[s]])
        }
        else{
            if (show.totals)
                sum <- data.frame(Total=object$summary.totals[[s]],stringsAsFactors = FALSE)
        }
        if ((show.missing!="never") && (show.missing=="always" || any(object$missing$totals[[s]]>0))){
            if (!show.totals){
                if (is.null(object$groups)){
                    miss <- object$missing$totals[[s]]
                }else{
                    miss <- unlist(object$missing$group[[s]])
                }
            } else{
                miss <- c(unlist(object$missing$group[[s]]),object$missing$totals[[s]])
            }
        } else{
            miss <- NULL
        }
        sum <- rbind(sum,miss)
        if (object$vartype[[s]]=="factor"){
            lev <- object$xlevels[[s]]
            if ((is.logical(drop.reference) && drop.reference[1]==TRUE)
                || (is.character(drop.reference) && (s %in% drop.reference))
                || (is.character(drop.reference) && drop.reference[1]=="binary" && length(lev)==2)
                || (is.character(drop.reference) && drop.reference[1]=="all")){
                ## remove redundant line for reference level
                lev <- lev[-1]
                sum <- sum[-1,,drop=FALSE]
            }
        } else{
            if (object$vartype[[s]]=="Q") 
                lev <- gsub("\\(x\\)","",object$Q.format)
            else
                lev <- gsub("\\(x\\)","",object$summary.format)
        }
        if (!is.null(miss)) lev <- c(lev,"missing")
        if (show.pvalues && !is.null(object$p.values)){
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
    # }}}
    # {{{ column names and n
    if (length(n)>0 && !(is.null(object$groups))){
        if (n=="inNames"){
            object$groups <- paste(object$groups," (n=",object$n.groups[-length(object$n.groups)],")",sep="")
        }
        else{
            XXtab <- rbind(c("n","",object$n.groups,""),XXtab)
        }
    }
    if (is.null(object$groups)){
        colnames(XXtab) <- c("Variable","Levels","Value")
        XXtab$Variable <- as.character(XXtab$Variable)
        XXtab$Levels <- as.character(XXtab$Levels)
        totalName <- "Total"
        pname <- NULL
    }
    else{
        if ((show.pvalues==TRUE) && !is.null(object$p.values)){
            if (tolower(as.character(object$compare.groups)) %in% c("cox","logistic"))
                pname <- paste("p-value ","(",object$compare.groups,")",sep="")
            else
                pname <- "p-value"
        }else pname <- NULL
        if (show.totals[[1]]==TRUE){
            if (length(n)>0 && (n=="inNames"))
                totalName <- paste("Total"," (n=",object$n.groups[length(object$n.groups)],")",sep="")
            else
                totalName <- "Total"
        } else totalName <- NULL
    }
    colnames(XXtab) <- c("Variable","Level",object$groups,totalName,pname)
    # }}}
    # {{{ labels & units
    class(XXtab) <- c("summary.univariateTable","data.frame")
    XXtab <- do.call(labelUnits,c(list(x=XXtab),list(...),object$labels))
    # }}}
    rownames(XXtab) <- NULL
    XXtab
}

