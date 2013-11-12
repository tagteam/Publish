##' @S3method publish glm
publish.glm <- function(object,
                        digits=2,
                        pvalDigits=4,
                        eps=0.0001,
                        missing=NULL,
                        sel=NULL,
                        drop,
                        intercept=0,
                        print=TRUE,
                        transform=NULL,
                        profile=TRUE,
                        pvalue.stars=FALSE,
                        ci.format,
                        missing.string="--",
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
    if (is.null(missing))
        missing <- any(Nmiss>0)
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
        if (is.null(sel)){
            sel <- c("OddsRatio","StandardError","CI.95","pValue","Missing")
        }
        names(x)[1] <- "OddsRatio"
        x$OddsRatio=format(exp(x$OddsRatio),digits=digits,nsmall=digits)
        ciX <- exp(ciX)
    }
    else{
        x$Estimate=format(x$Estimate,digits=digits,nsmall=digits)
        if (is.null(sel)){
            sel <- c("Estimate","StandardError","CI.95","pValue","Missing")
        }
    }
    ## x$CI.95=format.ci(ciX[,1],ciX[,2],digits=digits,style=ci.style)
    x$CI.95=apply(ciX[,c(1,2)],1,function(x){
        sprintf(ci.format,x[1],x[2])})
    x$tValue=format(x$tValue,digits=digits,nsmall=digits)
    if (pvalue.stars==TRUE)
        x$pValue <- symnum(x$pValue,corr = FALSE,na = FALSE,cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),symbols = c("***", "**", "*", ".", " "))
    ## x$pValue=sapply(x$pValue,format.pval,digits=pvalDigits,eps=eps)
    else
        x$pValue=sapply(x$pValue,format.pval,digits=pvalDigits,eps=eps)
    # }}}
    # {{{ treat predictors and missings
    factorLevels <- object$xlevels
    factorNames <- names(factorLevels)
    ## oldnames <- lapply(1:length(factorLevels),function(i){
    ## paste(factorNames[i],factorLevels[[i]],sep="")})
    if (length(factorLevels)){
        newnames <- lapply(1:length(factorLevels),function(i){
            paste(factorNames[i],factorLevels[[i]],sep="=")})
        names(newnames) <- factorNames
    }
    # {{{ if intercept take it out
    if (found <- match("(Intercept)",rownames(x),nomatch=0)){
        inter <- x[found,,drop=FALSE]
        x <- x[-found,,drop=FALSE]
        if (missing)
            inter <- cbind(inter,Missing=missing.string)
    }
    # }}}
    # {{{ missing values

  # }}}
  # {{{ variable names factor levels
  splitty <- unlist(lapply(varNames,function(vn){
    ## the rowname is either vnLev or vn
    rep(vn,length(grep(paste("^",vn,sep=""),rownames(x))))
  }))
  xlist <- split(x,factor(splitty,levels=unique(splitty)))
  xfixed <- lapply(1:length(xlist),function(v){
    ## factors
    if (found <- match(names(xlist)[v],factorNames,nomatch=0)){
      if (missing){
        add <- c(ifelse(logisticRegression,1,0),rep(missing.string,ncol(x)-1),Missing=Nmiss[v])
        old <- cbind(xlist[[v]],Missing=rep(missing.string,NROW(xlist[[v]])))
        old$Missing <- as.character(old$Missing)
        xlist[[v]] <- rbind(add,old)
        xlist[[v]]
      }
      else{
        xlist[[v]] <- rbind(c(ifelse(logisticRegression,1,0),rep(missing.string,ncol(x)-1)),
                            xlist[[v]])
      }
      rownames(xlist[[v]]) <- newnames[[found]]
      xlist[[v]]
    }
    else{ ## numeric
      if (missing)
        cbind(xlist[[v]],Missing=Nmiss[v])
      else
        xlist[[v]]
    }
  })
  x <- do.call("rbind",xfixed)
  ## if (missing==TRUE && length(object$na.action)>0){ 
  ## x$missing=0
  ## }
  ## if (match("missing",sel,nomatch=FALSE) && length(object$na.action)>0){
  ## x$missing=length(object$na.action)
  ## }
  # }}}
  # {{{ add intercept if wanted
  if (intercept!=0)
    x <- rbind(inter,x)
  # }}}
  
  # {{{ remove unwanted columns
  found <- match(sel,names(x),nomatch=FALSE)!=0
  sel <- sel[found]
  if (!missing(drop)) x=x[-drop,sel]
  else
    x=x[,sel]
  if (print==TRUE){
    out <- publish.matrix(x,col1name="Factor",...)
  }
  # }}}
  invisible(x)  
}
##' @S3method publish lm
publish.lm <- publish.glm
