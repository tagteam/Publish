##' Tabulize the part of the result of a generalized linear regression analysis which is commonly shown in publications. 
##'
##' For logistic regression, i.e. when family = binomial, return odds ratio's with confidence intervals.
##' @title Tabulize regression coefficients with confidence intervals and p-values.
##' @S3method publish glm
##' @param object
##' @param digits
##' @param pvalue.digits
##' @param eps
##' @param missing
##' @param output.columns Select which parameters of the result are shown in the table. Defaults to \code{c("Estimate","CI.95","pValue","Missing")} for linear regression and to \code{c("OddsRatio","CI.95","pValue","Missing")} for logistic regression. Can also include \code{StandardError}.
##' @param drop
##' @param intercept
##' @param print
##' @param transform
##' @param profile
##' @param pvalue.stars
##' @param ci.format
##' @param showResponseTable
##' @param ...
##' @return Table with regression coefficients, confidence intervals and p-values.
##' @author Thomas Alexander Gerds
##' @export
publish.glm <- function(object,
                        digits=2,
                        pvalue.digits=4,
                        eps=0.0001,
                        missing=NULL,
                        output.columns=NULL,
                        drop,
                        intercept=0,
                        print=TRUE,
                        transform=NULL,
                        profile=TRUE,
                        pvalue.stars=FALSE,
                        ci.format,
                        style="extraline",
                        showResponseTable=FALSE,
                        ...){
    # {{{ formula and missing values?
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
    varNames <- all.vars(formula)[-1]
    Nmiss <- sapply(varNames,function(v){sum(is.na(data[,v]))})
    names(Nmiss) <- varNames
    if (is.null(missing)) missing <- "ifany"
    missing <- switch(as.character(missing),
                      "ifany"=any(Nmiss>0),
                      "always"=TRUE,
                      "never"=FALSE)
    # }}}
    # {{{ logisticRegression?
    logisticRegression <- (!is.null(object$family$family) && object$family$family=="binomial")
    if (missing(ci.format)){
        if (logisticRegression)
            ci.format <- paste("[",paste("%1.",digits,"f",sep=""),"-",paste("%1.",digits,"f",sep=""),"]",sep="")
        else
            ci.format <- paste("[",paste("%1.",digits,"f",sep=""),";",paste("%1.",digits,"f",sep=""),"]",sep="")
    }
    # }}}
    # {{{ Table response for logisticRegression 
    if (print==TRUE && logisticRegression && showResponseTable){
        D <- object$model
        response <- D[,as.character(formula[[2]])]
        tmp <- as.matrix(table(response))
        tmp <- cbind(rownames(tmp),tmp)
        colnames(tmp) <- c("Response","N")
        if (missing)
            tmp <- rbind(tmp,c("Missing",length(object$na.action)))
        publish(tmp,rownames=FALSE,...)
        cat("\n")
    }
    # }}}
    # {{{ prepare table with confidence limits
    x <- data.frame(summary(object)$coefficients)
    names(x) <- c("Estimate","StandardError","tValue","pValue")
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
            ## output.columns <- c("OddsRatio","StandardError","CI.95","pValue","Missing")
            if (missing)
                output.columns <- c("Variable","Units","Missing","OddsRatio","CI.95","pValue")
            else
                output.columns <- c("Variable","Units","OddsRatio","CI.95","pValue")
        }
        names(x)[1] <- "OddsRatio"
        x$OddsRatio=format(exp(x$OddsRatio),digits=digits,nsmall=digits)
        ciX <- exp(ciX)
    }
    else{
        x$Estimate=format(x$Estimate,digits=digits,nsmall=digits)
        if (is.null(output.columns)){
            ## output.columns <- c("Estimate","StandardError","CI.95","pValue","Missing")
            if (missing)
                output.columns <- c("Variable","Units","Missing","Estimate","CI.95","pValue")
            else
                output.columns <- c("Variable","Units","Estimate","CI.95","pValue")
        }
    }
    ## x$CI.95=format.ci(ciX[,1],ciX[,2],digits=digits,style=ci.style)
    x$CI.95=apply(ciX[,c(1,2)],1,function(x){
        sprintf(ci.format,x[1],x[2])})
    x$tValue=format(x$tValue,digits=digits,nsmall=digits)
    if (pvalue.stars==TRUE)
        x$pValue <- symnum(x$pValue,corr = FALSE,na = FALSE,cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),symbols = c("***", "**", "*", ".", " "))
    ## x$pValue=sapply(x$pValue,format.pval,digits=pvalue.digits,eps=eps)
    else
        x$pValue=sapply(x$pValue,
            format.pval,
            digits=pvalue.digits,
            eps=eps)
    # }}}
    rt <- fixRegressionTable(x,
                             varnames=varNames,
                             factorlevels=object$xlevels,
                             reference.style=style,
                             reference.value=ifelse(logisticRegression,1,0),
                             nmiss=Nmiss,
                             intercept=intercept)
    # }}}
    # {{{ remove unwanted columns
    found <- match(output.columns,names(rt),nomatch=FALSE)!=0
    output.columns <- output.columns[found]
    if (!missing(drop)) rt=rt[-drop,output.columns]
    else
        rt=rt[,output.columns]
    names(rt)[names(rt)=="pValue"] <- "p-value"
    rt <- labelUnits(rt,...)
    if (print==TRUE){
        out <- publish(rt,rownames=FALSE,...)
    }
    # }}}
    invisible(rt)  
}
##' @S3method publish lm
publish.lm <- publish.glm
