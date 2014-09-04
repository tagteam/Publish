##' Tabulize the part of the result of a Cox regression analysis which is commonly shown in publications.
##'
##' Transforms the log hazard ratios to hazard ratios and returns them with confidence limits and p-values.
##' @title Tabulize hazard ratios with confidence intervals and p-values.
##' @S3method publish coxph
##' @param object A \code{coxph} object.
##' @param digits
##' @param pvalue.digits
##' @param eps
##' @param pvalue.stars
##' @param showMissing
##' @param output.columns
##' @param print
##' @param ci.format
##' @param style
##' @param ...
##' @param scale Scaling factor multiplied to both the log-hazard
##' ratio and its standard-error. Useful to change the units of the
##' intepretation scale.
##' @return Table with hazard ratios, confidence intervals and p-values.
##' @author Thomas Alexander Gerds
##' @export
publish.coxph <- function(object,
                          digits=2,
                          pvalue.digits=4,
                          eps=.0001,
                          pvalue.stars=FALSE,
                          showMissing="ifany",
                          output.columns=NULL,
                          print=TRUE,
                          ci.format=paste("[",paste("%1.",digits,"f",sep=""),";",paste("%1.",digits,"f",sep=""),"]",sep=""),
                          style="extraline",
                          ...) {
    # {{{ formula, data 
    if (is.null(object$formula)){
        if (is.null(object$terms)){
            if (class(object$call$formula)=="name"){
                stop("Cannot extract the formula from object")
            }
            else{
                formula <- object$call$formula
            }
        } else{
            formula <- formula(object$terms)
        }
    }
    else{
        formula <- object$formula
    }
    if (is.null(object$data))
        data <- eval(object$call$data)
    else
        data <- object$data
    terms <- terms(formula)
    if (any(attr(terms,"order")>1)) stop("Unfortunately, higher order terms are not supported.\n We all hope that Klaus will fix this soon.")
    ## hack hack
    ## varNames <- all.vars(delete.response(terms(object$formula)))
    ff <- readFormula(object$formula,specials=c("cluster","strata"))
    varNames <- all.vars(ff$unSpec$formula)
    factors <- colnames(attr(terms,"factors"))
    clustvar <- all.vars(ff$cluster$formula)
    if (length(clustvar)){
        factors <- factors[factors != paste("cluster(",clustvar,")",sep="")]
    }
    log.e <- match(paste("log(",varNames,")",sep=""),factors,nomatch=FALSE)!=0
    log.2 <- match(paste("log(",varNames,", base = 2)",sep=""),factors,nomatch=FALSE)!=0
    unity <- match(varNames,factors,nomatch=FALSE)
    scale <- 1*(unity!=0) + 2 * (log.e!=0) + 3 * (log.2!=0)
    if (any(scale==0)) stop("Unfortunately, constructions like I(age>50) are not supported\nYou need to define the transformed variable in the data set before calling coxph.\n")
    scale <- as.character(factor(scale,
                                 levels=c(1,2,3),
                                 labels=c("","logarithmic","logarithmic base 2")))
    names(scale) <- varNames
    # }}}
    # {{{     missing values?
    Nmiss <- sapply(varNames,function(v){sum(is.na(data[,v]))})
    names(Nmiss) <- varNames
    if (is.null(showMissing)) showMissing <- "ifany"
    showMissing <- switch(as.character(showMissing),"ifany"=any(Nmiss>0),"always"=TRUE,"never"=FALSE)
    # }}}
    # {{{ confidence interval format
    ## if (missing(ci.format)){
    ## ci.format <- paste("[",paste("%1.",digits,"f",sep=""),";",paste("%1.",digits,"f",sep=""),"]",sep="")
    ## }
    # }}}
    # {{{ prepare table with confidence limits and p-values
    x <- data.frame(summary(object)$coefficients)
    if (length(clustvar)){
        names(x) <- c("logHR","HazardRatio","StandardError","robustStandardError","z","pValue")
        x$robustStandardError=format(x$robustStandardError,digits=digits,nsmall=digits)
    }
    else
        names(x) <- c("logHR","HazardRatio","StandardError","z","pValue")
    x$StandardError=format(x$StandardError,digits=digits,nsmall=digits)
    x$HazardRatio=format(x$HazardRatio,digits=digits,nsmall=digits)
    x$logHR=format(x$logHR,digits=digits,nsmall=digits)
    ciX <- exp(confint(object))
    x$CI.95=apply(ciX[,c(1,2),drop=FALSE],1,function(x){
        sprintf(ci.format,x[1],x[2])})
    if (pvalue.stars==TRUE)
        x$pValue <- symnum(x$pValue,corr = FALSE,na = FALSE,cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),symbols = c("***", "**", "*", ".", " "))
    else
        x$pValue=sapply(x$pValue,
            format.pval,
            digits=pvalue.digits,
            eps=eps)
    # }}}
    # {{{ output columns
    if (is.null(output.columns)){
        if (showMissing)
            output.columns <- c("Variable","Units","Missing","HazardRatio","CI.95","pValue")
        else
            output.columns <- c("Variable","Units","HazardRatio","CI.95","pValue")
    }
    if (any((log.2 + log.e)>0)){
        output.columns <- c(output.columns[1],"Scale",output.columns[2:length(output.columns)])
    }
    # }}}
    # {{{ fix
    rt <- fixRegressionTable(x,
                             varnames=varNames,
                             factorlevels=object$xlevels,
                             reference.style=style,
                             reference.value=1,
                             scale=scale,
                             nmiss=Nmiss,
                             intercept=FALSE)
    # }}}
    # {{{ remove unwanted columns and print
    found <- match(output.columns,names(rt),nomatch=FALSE)!=0
    output.columns <- output.columns[found]
    rt <- rt[,output.columns]
    names(rt)[names(rt)=="pValue"] <- "p-value"
    rt <- labelUnits(rt,...)
    if (print==TRUE){
        publish(rt,rownames=FALSE,...)
    }
    # }}}
    invisible(rt)  
}

