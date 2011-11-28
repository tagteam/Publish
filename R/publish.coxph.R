publish.coxph <- function(object,
                          conf.int = 0.95,
                          scale = 1,
                          digits = 2,
                          pvalDigits=4,
                          eps=.0001,
                          StandardError=FALSE,
                          print=TRUE,
                          missing=TRUE,
                          ...) {
  beta <- object$coef
  ##   print(names(beta))
  ##   varnames <- attr(mRD$terms,"term.labels")
  ##   lapply(varnames,function(v){
  ##   }
  nabeta <- !(is.na(beta))
  beta2 <- beta[nabeta]
  if (is.null(beta) | is.null(object$var)) stop("Input is not valid")
  se <- sqrt(diag(object$var))
  if (!is.null(object$naive.var)) nse <- sqrt(diag(object$naive.var))
  z <- qnorm((1 + conf.int)/2, 0, 1)
  beta <- beta * scale
  se <- se * scale
  if (StandardError==TRUE)
    x <- data.frame("Hazard ratio"=format(round(exp(beta),digits)),
                      "Standard error"=format(round(se,digits)),
                      "CI.95"=paste("[",format(round(exp(beta - z * se),2)),";",format(round(exp(beta + z * se),2)),"]",sep=""),
                      "P-value"=sapply(1 - pchisq((beta/se)^2, 1),format.pval,digits=pvalDigits,eps=eps),stringsAsFactors=FALSE)
  else
    x <- data.frame("Hazard ratio"=format(round(exp(beta),digits)),
                    "CI.95"=paste("[",format(round(exp(beta - z * se),2)),";",format(round(exp(beta + z * se),2)),"]",sep=""),
                    "P-value"=sapply(1 - pchisq((beta/se)^2, 1),format.pval,digits=pvalDigits,eps=eps),stringsAsFactors=FALSE)
  
  
  # {{{ treat predictors and missings
  varNames <- all.vars(delete.response(terms(object$formula)))
  if (missing){
    dd <- try(eval(object$call$data),silent=TRUE)
    if (class(dd)=="try-error"){
      missing <- FALSE
    }
    else{
      Nmiss <- sapply(varNames,function(v){sum(is.na(dd[,v]))})
      names(Nmiss) <- varNames
    }
  }
  factorLevels <- object$xlevels
  factorNames <- names(factorLevels)
  ## oldnames <- lapply(1:length(factorLevels),function(i){
  ## paste(factorNames[i],factorLevels[[i]],sep="")})
  newnames <- lapply(1:length(factorLevels),function(i){
    paste(factorNames[i],factorLevels[[i]],sep="=")})
  names(newnames) <- factorNames
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
        xlist[[v]] <- rbind(c(1,rep("--",ncol(x)-1)),
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
  invisible(x)
}

