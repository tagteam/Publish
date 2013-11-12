##' @S3method publish compRisk
publish.compRisk <- function(object,
                             conf.int = 0.95,
                             digits = 2,
                             pvalue.digits = 4,
                             eps=.0001,
                             mark.pval=FALSE,
                             ...) {
  # {{{ find covariates and factor levels 
  cvars <- all.vars(object$design$const$formula)
  tvars <- all.vars(object$design$nonpar$formula)
  Flevels <- object$factorLevels
  # }}}
  #timeConstCoef
  const.coef <- object$timeConstCoef
  const.se <- sqrt(diag(object$timeConstVar))
  wald <- const.coef/const.se
  waldp <- (1 - pnorm(abs(wald))) * 2
  format.waldp <- sapply(waldp,base:::format.pval,digits=pvalue.digits,eps=eps)
  # mark the pvalue in muse
  if (mark.pval){
    format.waldp <- sapply(1: length(waldp), function(p){
      if (waldp[p] < 1-conf.int) paste("**",format.waldp[p],"**",sep="") 
      else format.waldp[p]
    })    
  }
  names(format.waldp) <- names(waldp)
  format.waldp[const.se==0] <- NA
  zVal <- qnorm((1 + conf.int)/2, 0, 1)
  ###
  if (any(const.se==0))
    warning("Some standard errors are zero. It seems that the model did not converge")
  coefMat <- do.call("rbind",lapply(cvars,function(v){
    covname <- strsplit(v,":")[[1]][[1]]
    if (is.null(Flevels[[covname]])){
      ci=paste("[",format(exp(const.coef[v] - zVal * const.se[v]),digits=digits,nsmall=digits),";",format(exp(const.coef[v] + zVal * const.se[v]),digits=digits,nsmall=digits),"]",sep="")
      out <- c(v,signif(c(exp(const.coef[v])),digits),ci,format.waldp[v])
    }
    else{
      rlev <- object$refLevels[[covname]]
      out <- do.call("rbind",lapply(Flevels[[covname]],function(l){
        V <- paste(covname,l,sep=":")
        if (match(V,paste(covname,rlev,sep=":"),nomatch=FALSE))
          c(paste(covname,rlev,sep=":"),"--","--","--")
        else{
          ci=paste("[",format(exp(const.coef[V] - zVal * const.se[V]),digits=digits,nsmall=digits),";",format(exp(const.coef[V] + zVal * const.se[V]),digits=digits,nsmall=digits),"]",sep="")
        c(V,signif(c(exp(const.coef[V])),digits),ci,format.waldp[V])
        }
      }))
    }
  }))
  colnames(coefMat) <- c("Factor","CIR",paste("CI",conf.int*100,sep="."),"P-value")
  rownames(coefMat) <- rep("",NROW(coefMat))
  publish(coefMat,...,rownames=FALSE)
}                 
