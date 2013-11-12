##' Tabulize the part of the result of a Cox regression analysis which is commonly shown in publications.
##'
##' Transforms the log hazard ratios to hazard ratios and returns them with confidence limits and p-values.
##' @title Tabulize hazard ratios with confidence intervals and p-values.
##' @S3method publish coxph
##' @param object A \code{coxph} object.
##' @param conf.int Level of confidence. 
##' @param scale Scaling factor multiplied to both the log-hazard ratio and its standard-error. Useful to change the units of the intepretation scale.
##' @param digits 
##' @param ci.format
##' @param ci.digits
##' @param pvalue.digits
##' @param eps
##' @param StandardError
##' @param print
##' @param missing
##' @param sum
##' @param ...
##' @return Table with hazard ratios, confidence intervals and p-values.
##' @author Thomas Alexander Gerds
##' @export
publish.coxph <- function(object,
                          conf.int = 0.95,
                          scale = 1,
                          digits = 2,
                          ci.format=4,
                          ci.digits=2,
                          pvalue.digits=4,
                          eps=.0001,
                          StandardError=FALSE,
                          print=TRUE,
                          missing=TRUE,
                          sum=FALSE,
                          ...) {
  # {{{ create a data frame 

    beta <- object$coef
    nabeta <- !(is.na(beta))
    beta2 <- beta[nabeta]
    if (is.null(beta) | is.null(object$var)) stop("Input is not valid")
    se <- sqrt(diag(object$var))
    if (!is.null(object$naive.var)) nse <- sqrt(diag(object$naive.var))
    z <- qnorm((1 + conf.int)/2, 0, 1)
    beta <- beta * scale
    se <- se * scale
    if (StandardError==TRUE)
        x <- data.frame("Hazard ratio"=format(exp(beta),digits=digits,nsmall=digits),
                        "Standard error"=format(se,digits=digits,nsmall=digits),
                        "CI.95"=format.ci(lower=exp(beta - z * se),2,upper=exp(beta + z * se),style=ci.format,digits=ci.digits),
                            ## paste("[",format(round(exp(beta - z * se),2)),";",format(round(exp(beta + z * se),2)),"]",sep=""),
                        "P-value"=sapply(1 - pchisq((beta/se)^2, 1),format.pval,digits=pvalue.digits,eps=eps),stringsAsFactors=FALSE)
    else
        x <- data.frame("Hazard ratio"=format(exp(beta),digits=digits,nsmall=digits),
                        "CI.95"=format.ci(lower=exp(beta - z * se),2,upper=exp(beta + z * se),style=ci.format,digits=ci.digits),
                        ## "CI.95"=paste("[",format(round(exp(beta - z * se),2)),";",format(round(exp(beta + z * se),2)),"]",sep=""),
                        "P-value"=sapply(1 - pchisq((beta/se)^2, 1),format.pval,digits=pvalue.digits,eps=eps),stringsAsFactors=FALSE)

  # }}}
  # {{{ missing values 
  varNames <- all.vars(delete.response(terms(object$formula)))
  if (missing){
    dd <- try(eval(object$call$data),silent=TRUE)
    if (class(dd)=="function" || class(dd)=="try-error" || is.null(dd)){
      missing <- FALSE
    }
    else{
      Nmiss <- sapply(varNames,function(v){sum(is.na(dd[,v]))})
      names(Nmiss) <- varNames
      if (all(Nmiss==0))
        missing <- FALSE
    }
  }
  # }}}
  # {{{ factors with a reference level
  factorLevels <- object$xlevels
  factorNames <- names(factorLevels)
  ## oldnames <- lapply(1:length(factorLevels),function(i){
  ## paste(factorNames[i],factorLevels[[i]],sep="")})
  newnames <- lapply(1:length(factorLevels),function(i){
    paste(factorNames[i],factorLevels[[i]],sep="=")})
  names(newnames) <- factorNames
  # {{{ summary: mean (sd) for numeric, count for factors with levels
  if (sum){
    dd <- try(eval(object$call$data),silent=TRUE)
    if (class(dd)=="function" || class(dd)=="try-error" || is.null(dd)){
      sum <- FALSE
    }
    else{
      sumInfo <- lapply(varNames,function(v){
        if (v %in% names(object$xlevels)){
          table(dd[,v])
        }
        else{
          paste(format(mean(dd[,v],na.rm=TRUE),digits=digits,nsmall=digits)," (",format(sd(dd[,v],na.rm=TRUE),digits=digits,nsmall=digits),")",sep="")
        }})
      names(sumInfo) <- varNames
    }
  }
  # }}}
  # {{{ if intercept take it out

  if (found <- match("(Intercept)",rownames(x),nomatch=0)){
    inter <- x[found,,drop=FALSE]
    x <- x[-found,,drop=FALSE]
    if (missing)
      inter <- cbind(inter,Missing="--")
  }

  # }}}
  splitty <- unlist(lapply(varNames,function(vn){
    rep(vn,length(grep(vn,rownames(x))))
  }))
  xlist <- split(x,factor(splitty,levels=unique(splitty)))
  xfixed <- lapply(1:length(xlist),function(v){
    ## factors
    if (found <- match(names(xlist)[v],factorNames,nomatch=0)){
      if (missing){
        add <- c(1,rep("--",ncol(x)-1),Missing=Nmiss[v])
        old <- cbind(xlist[[v]],Missing=rep("--",NROW(xlist[[v]])))
        old$Missing <- as.character(old$Missing)
        xlist[[v]] <- rbind(add,old)
        xlist[[v]]
      }
      else{
        xlist[[v]] <- rbind(c(1,rep("--",ncol(x)-1)), xlist[[v]])
      }
      rownames(xlist[[v]]) <- newnames[[found]]
      xlist[[v]]
    }
    else{ ## numeric
      if (missing)
        xlist[[v]] <- cbind(xlist[[v]],Missing=Nmiss[v])
      else
        xlist[[v]]
    }
    if (sum){
      ## if (v %in% names(object$xlevels)){
      xlist[[v]] <- cbind("N or mean(sd)"=as.character(sumInfo[[v]]),xlist[[v]])
      if (length(names(object$xlevels))==length(varNames))
        names(xlist[[v]])[1] = "N"
      else 
        if (length(names(object$xlevels))==0)
          names(xlist[[v]])[1] = "mean(sd)"
    }
    xlist[[v]]
  })
  x <- do.call("rbind",xfixed)
  ## if (missing==TRUE && length(object$na.action)>0){ 
  ## x$missing=0
  ## }
  ## if (match("missing",sel,nomatch=FALSE) && length(object$na.action)>0){
  ## x$missing=length(object$na.action)
  ## }
  # }}}
  # {{{ print result
  if (print==TRUE){
    publish(x,...)
    if (length(z <- object$na.action)){
      cat("\nObservations deleted due to missing values:\n")
      if (attr(z,"class")=="delete"){
        zz <- cbind(names(z$nmiss[z$nmiss!=0]),matrix(z$nmiss[z$nmiss!=0],ncol=1))
        colnames(zz) <- c("Factor","Missing")
        publish(zz,col1name="Factor",rownames=FALSE)
      }
      else{
        publish(naprint(z))
      }
    }
  }
  # }}}
  invisible(x)
}

