##' Tabulize the part of the result of a generalized linear regression analysis which is commonly shown in publications. 
##'
##' For logistic regression, i.e. when family = binomial, return odds ratio's with confidence intervals.
##' @title Tabulize regression coefficients with confidence intervals and p-values.
##' @S3method publish glm
##' @param object A \code{glm} object.
##' @param digits Rounding digits for all numbers but the p-values.
##' @param pvalue.digits Rounding digits for p-values.
##' @param eps passed to format.pval
##' @param pvalue.stars If \code{TRUE} significance is indicated by
##' stars instead of p-values.
##' @param showMissing If \code{TRUE} show number of missing values in
##' table
##' @param output.columns Select which parameters of the result are
##' shown in the table. Defaults to
##' \code{c("Coefficient","CI.95","pValue","Missing")} for linear
##' regression and to \code{c("OddsRatio","CI.95","pValue","Missing")}
##' for logistic regression. Can also include \code{StandardError}.
##' @param intercept If \code{FALSE} suppress intercept
##' @param print If \code{FALSE} do not print results.
##' @param transform Transformation for regression coefficients.
##' @param profile For logistic regression only. If \code{FALSE} run
##' with Wald confidence intervals instead of waiting for profile
##' confidence intervals.
##' @param ci.format Format for confidence intervals passed to
##' \code{\link{sprintf}} with two arguments: the lower and the upper
##' limit.
##' @param reference Style for showing results for categorical
##' variables. If \code{"extraline"} show an additional line for the
##' reference category.
##' @param ... passed to labelUnits
##' @return Table with regression coefficients, confidence intervals and p-values.
##' @author Thomas Alexander Gerds
##' @examples
##' data(Diabetes)
##' f = glm(bp.2s~frame+gender+age,data=Diabetes)
##' publish(f)
##' publish(f,reference="inline")
##' publish(f,pvalue.stars=TRUE)
##' publish(f,ci.format="(%1.1f,%1.1f)")
##' 
##' @export
publish.glm <- function(object,
                        digits=2,
                        pvalue.digits=4,
                        eps=0.0001,
                        pvalue.stars=FALSE,
                        showMissing=NULL,
                        output.columns=NULL,
                        intercept=0,
                        print=TRUE,
                        transform=NULL,
                        profile=TRUE,
                        ci.format=NULL,
                        reference="extraline",
                        ...){
    if (is.null(ci.format)) ci.format <- paste("[",paste("%1.",digits,"f",sep=""),";",paste("%1.",digits,"f",sep=""),"]",sep="")
    # {{{ formula, data and 
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
    if (any(attr(terms,"order")>1)) stop("Unfortunately, higher order terms are not supported. We all hope that Klaus will fix this soon.")
    varNames <- all.vars(formula)[-1]
    factors <- colnames(attr(terms,"factors"))
    log.e <- match(paste("log(",varNames,")",sep=""),factors,nomatch=FALSE)!=0
    log.2 <- match(paste("log(",varNames,", base = 2)",sep=""),factors,nomatch=FALSE)!=0
    unity <- match(varNames,factors,nomatch=FALSE)
    scale <- 1*(unity!=0) + 2 * (log.e!=0) + 3 * (log.2!=0)
    if (any(scale==0)) stop("Unfortunately, constructions like I(age>50) are not supported\nYou need to define the transformed variable in the data set before calling coxph.\n")
    scale <- as.character(factor(scale,levels=c(1,2,3),labels=c("","logarithmic","logarithmic base 2")))
    names(scale) <- varNames
    # }}}
    # {{{ missing values?
    Nmiss <- sapply(varNames,function(v){sum(is.na(data[,v]))})
    names(Nmiss) <- varNames
    if (is.null(showMissing)) showMissing <- "ifany"
    showMissing <- switch(as.character(showMissing),
                          "ifany"=any(Nmiss>0),
                          "always"=TRUE,
                          "never"=FALSE)
    # }}}
    # {{{ logisticRegression?
    logisticRegression <- (!is.null(object$family$family) && object$family$family=="binomial")
    # }}}
    # {{{ confidence interval format
    if (missing(ci.format)){
        ci.format <- paste("[",paste("%1.",digits,"f",sep=""),";",paste("%1.",digits,"f",sep=""),"]",sep="")
    }
    # }}}
    # {{{ prepare table with confidence limits and p-values
    x <- data.frame(summary(object)$coefficients)
    names(x) <- c("Coefficient","StandardError","tValue","pValue")
    if (profile==TRUE)
        ciX <- suppressMessages(confint(object))
    else{
        ctable <- coef(summary(object))
        lower <- ctable[,1]- qnorm(.975)*ctable[,2]
        upper <- ctable[,1]+ qnorm(.975)*ctable[,2]
        ciX <- cbind(lower,upper)
    }
    x$StandardError=format(x$StandardError,digits=digits,nsmall=digits)
    if (logisticRegression||((!is.null(transform)) &&(transform=="exp"))){
        if (is.null(output.columns)){
            if (showMissing)
                output.columns <- c("Variable","Units","Missing","OddsRatio","CI.95","pValue")
            else
                output.columns <- c("Variable","Units","OddsRatio","CI.95","pValue")
        }
        names(x)[1] <- "OddsRatio"
        ## format the intercept separately
        ic.or <- x$OddsRatio[[1]]
        x$OddsRatio <- c(format(ic.or,,digits=digits,nsmall=digits),
                         format(exp(x$OddsRatio)[-1],digits=digits,nsmall=digits))
        ciX <- exp(ciX)
    }
    else{
        x$Coefficient=format(x$Coefficient,digits=digits,nsmall=digits)
        if (is.null(output.columns)){
            if (showMissing)
                output.columns <- c("Variable","Units","Missing","Coefficient","CI.95","pValue")
            else
                output.columns <- c("Variable","Units","Coefficient","CI.95","pValue")
        }
    }
    x$CI.95=apply(ciX[,c(1,2)],1,function(x){
        sprintf(ci.format,x[1],x[2])})
    x$tValue=format(x$tValue,digits=digits,nsmall=digits)
    if (pvalue.stars==TRUE)
        x$pValue <- symnum(x$pValue,corr = FALSE,na = FALSE,cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),symbols = c("***", "**", "*", ".", " "))
    else
        x$pValue=sapply(x$pValue,
            format.pval,
            digits=pvalue.digits,
            eps=eps)
    # }}}
    # {{{ fix regression table
    rt <- fixRegressionTable(x,
                             varnames=varNames,
                             factorlevels=object$xlevels,
                             reference.style=reference,
                             reference.value=ifelse(logisticRegression,1,0),
                             scale=scale,
                             nmiss=Nmiss,
                             intercept=intercept)
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
    attributes(rt) <- c(attributes(rt),list(CI=ciX,
                                            OR=x$OddsRatio))
    invisible(rt)
}
##' @S3method publish lm
publish.lm <- publish.glm
