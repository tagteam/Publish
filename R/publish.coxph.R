##' Tabulize the part of the result of a Cox regression analysis which is commonly shown in publications.
##'
##' Transforms the log hazard ratios to hazard ratios and returns them with confidence limits and p-values.
##' If explanatory variables are log transformed or log2 transformed, a scaling factor is multiplied to both the log-hazard
##' ratio and its standard-error. 
##' @title Tabulize hazard ratios with confidence intervals and p-values.
##' @param object A \code{coxph} object.
##' @param digits Rounding digits for all numbers but the p-values.
##' @param pvalue.digits Rounding digits for p-values.
##' @param eps passed to format.pval
##' @param pvalue.stars If \code{TRUE} significance is indicated by
##' stars instead of p-values.
##' @param ci.format Format for confidence intervals passed to
##' \code{\link{sprintf}} with two arguments: the lower and the upper
##' limit.
##' @param showMissing If \code{TRUE} show number of missing values in
##' table
##' @param output.columns Select output columns
##' @param print If \code{FALSE} do not print results.
##' @param reference Style for showing results for categorical
##' variables. If \code{"extraline"} show an additional line for the
##' reference category.
##' @param ... passed to labelUnits
##' @return Table with hazard ratios, confidence intervals and p-values.
##' @author Thomas Alexander Gerds
##' @examples
##' library(survival)
##' data(pbc)
##' pbc$edema <- factor(pbc$edema,levels=c("0","0.5","1"),labels=c("0","0.5","1"))
##' fit = coxph(Surv(time,status!=0)~age+sex+edema+log(bili)+log(albumin)+log(protime),
##'             data=pbc)
##' publish(fit)
##' fit2 = coxph(Surv(time,status!=0)~age+sex+edema+log(bili,base=2)+log(albumin)+log(protime),
##'     data=pbc)
##' publish(fit2)
##'
##' # with cluster variable
##' fit3 = coxph(Surv(time,status!=0)~age+cluster(sex)+edema+log(bili,base=2)+log(albumin)+log(protime),
##'     data=pbc)
##' publish(fit3)
##'
##' # with strata and cluster variable
##' fit4 = coxph(Surv(time,status!=0)~age+cluster(sex)+strata(edema)+log(bili,base=2)+log(albumin)+log(protime),
##'     data=pbc)
##' publish(fit4)
##' 
##' @export
publish.coxph <- function(object,
                          confint.method,
                          pvalue.method,
                          digits=c(2,4),
                          eps=0.0001,
                          print=TRUE,
                          ci.format=NULL,
                          factor.reference="extraline",
                          units=NULL,
                          ...){
    if (missing(confint.method)) confint.method="default"
    if (missing(pvalue.method))
        pvalue.method=switch(confint.method,
            "robust"={"robust"},
            "simultaneous"={"simultaneous"},
            "default")
    spec <- attr(terms(object),"specials")
    cluster <- spec$cluster-1
    strata <- spec$strata-1
    # if (!is.null(cluster)) cluster <- cluster-1
    rt <- regressionTable(object,
                          noterms=c(cluster,strata),
                          confint.method=confint.method,
                          factor.reference=factor.reference,
                          units=units)
    srt <- summary.regressionTable(rt,
                                   digits=digits,
                                   print=FALSE)
    if (print==TRUE)
        publish(srt,...)
    invisible(srt)
}


publish.coxph1 <- function(object,
                           digits=2,
                           pvalue.digits=4,
                           eps=.0001,
                           pvalue.stars=FALSE,
                           ci.format=NULL,
                           showMissing="ifany",
                           output.columns=NULL,
                           print=TRUE,
                           reference="extraline",
                           ...) {
    if (is.null(ci.format)) ci.format <- "[u;l]"
    # {{{ formula, data
    terms <- object$terms
    if (is.null(terms)){
        if (class(object$call$formula)=="name"){
            stop("Cannot extract the formula from coxph object")
        }
    }else{
        formula <- formula(terms)
    }
    if (any(attr(terms,"order")>1))
        stop("Unfortunately, higher order terms are not supported.\n We all hope that Klaus will fix this soon.")
    data <- object$data
    if (is.null(data))
        data <- eval(object$call$data)
    rhs <- formula(delete.response(object$terms))
    varnames <- attr(terms(rhs),"term.labels")
    log.scale <- prodlim::parseSpecialNames(varnames,special="log",arguments=list("base"=exp(1)))
    strata <- prodlim::parseSpecialNames(varnames,special="strata")
    cluster <- prodlim::parseSpecialNames(varnames,special="cluster")
    X <- specialFrame(formula=rhs,
                      data=data,
                      unspecialsDesign=FALSE,
                      specialsFactor=FALSE,
                      specials=c("strata","cluster"),
                      specialsDesign=FALSE)
    varNames <- colnames(X$design)
    factors <- colnames(attr(terms,"factors"))
    clustvar <- if (!is.null(X$cluster)) colnames(X$cluster) else NULL
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
    ## warning("FIXME: need to repair information on missing values per variable")
    ## Nmiss <- sapply(varNames,function(v){sum(is.na(data[,v]))})
    Nmiss <- sapply(varNames,function(v) 0)
    names(Nmiss) <- varNames
    ## if (is.null(showMissing)) showMissing <- "ifany"
    ## showMissing <- switch(as.character(showMissing),"ifany"=any(Nmiss>0),"always"=TRUE,"never"=FALSE)
    showMissing <- FALSE
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
        formatCI(lower=x[1],upper=x[2],digits=digits,format=ci.format)
        ## sprintf(ci.format,x[1],x[2])
    })
    if (pvalue.stars==TRUE)
        x$pValue <- symnum(x$pValue,
                           corr = FALSE,
                           na = FALSE,
                           cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                           symbols = c("***", "**", "*", ".", " "))
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
                             reference.style=reference,
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
        if (pvalue.stars==TRUE)
            cat("\nSignif. codes:  0 '***'0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
    }
    # }}}
    invisible(rt)  
}


#----------------------------------------------------------------------
### publish.coxph.R ends here
