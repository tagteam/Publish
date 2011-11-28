# {{{ header
crossTable <- function(formula,
                       data,
                       asFactor="",
                       asNumeric="",
                       dignum=2,
                       printMissing=TRUE,
                       Totals=TRUE,
                       pValues=FALSE,
                       pvalMethod="ANOVA",
                       refLevel,
                       sd=TRUE,
                       se=FALSE,
                       minmax=FALSE,
                       median=FALSE,
                       IQR=median,
                       freq=TRUE,
                       sampleSize=TRUE,
                       eps=0.0001,
                       pdigits=4,
                       longlevelnames=TRUE,
                       transpose=FALSE){
  # }}}
  # {{{ specify factor and split data into groups 
  stopifnot(is.logical(longlevelnames))
  m <- model.frame(formula,data,na.action=na.pass)
  theFactor <- model.response(m)
  Fname <- names(m)[1]
  if (!is.factor(theFactor)) theFactor <- factor(theFactor)
  theLevels <- levels(theFactor)
  if (missing(refLevel)) refLevel <- theLevels[1]
  NL <- length(theLevels)
  rhs <- formula(delete.response(terms(formula)))
  type <- getType(formula=rhs,asFactor=asFactor,asNumeric=asNumeric,order=FALSE,data=m)
  groups <- split(m,theFactor)
  # }}}
  # {{{ adding the totals
   if (Totals) {
    groups <- c(groups,list(m))
    theLevels <- c(theLevels,"Total")
    NL <- NL+1
  }
  # }}}
  # {{{ in each sub-group call basetable
  baseL <- lapply(groups,function(x){
    if (median)
      baseTableMedian(formula=rhs,data=x,freq=freq,method=1,order=FALSE,minmax=minmax,longlevelnames=longlevelnames,printMissing=printMissing)
    else
      baseTable2(formula=rhs,asFactor=asFactor,asNumeric=asNumeric,data=x,sd=sd,se=se,freq=freq,method=1,order=FALSE,minmax=minmax,median=median,IQR=IQR,longlevelnames=longlevelnames,printMissing=printMissing,Totals=Totals)
  })
  factorNames <- baseL[[1]][,1]
  outL <- lapply(baseL,function(xxx){
    xxx[,grep("Mean|Freq|Median|Min|SE",names(xxx)),drop=TRUE]
  })
  # }}}
  # {{{ adding the sample size
  out <- do.call("rbind",outL)
  nnn <- sapply(groups,NROW)
  out <- data.frame(cbind(nnn,out),stringsAsFactors=FALSE)
  # }}}
  # {{{ adding the p-values

  if (pValues){
    if (any(match("ANOVA",pvalMethod,nomatch=FALSE))){
      pVal <- lapply(names(type),function(nn){
        ff <- formula(paste(nn,"~",Fname))
        if (type[nn]=="numeric"){
          pf <- anova(lm(ff,data=m))$"Pr(>F)"[1]
          pf <- format.pval(pf,digits=pdigits,eps=eps)
        }
        else{
          if (type[nn]=="character") m[[nn]] <- factor(m[[nn]])
          ## pf <- chisq.test(m[[nn]],m[[Fname]])$p.value
          pf <- chisq.test(m[[nn]],m[[Fname]])$p.value
          pf <- c(rep("--",length(levels(m[[nn]]))-1),
                  format.pval(pf,digits=pdigits,eps=eps))
                  ## signif(pf,digits=2))
        }
        pf
      })
      out <- data.frame(rbind(out,c("",unlist(pVal))),stringsAsFactors=FALSE)
    }
    if (any(match("Control",pvalMethod,nomatch=FALSE))){
      stopifnot(match(refLevel,theLevels,nomatch=FALSE)!=0)
      controlPval <- lapply(theLevels,function(ll){
        if (ll==refLevel || ll=="Total")
          "--"
        else{
          dd <- m[m[,Fname] %in% c(ll,refLevel),]
          dd[,Fname] <- factor(dd[,Fname])
          ## print(table(dd[[Fname]]))
          ## print(names(type))
          pVal <- do.call("rbind",lapply(names(type),function(nn){
            ff <- formula(paste(nn,"~",Fname))
            fam <- if (type[nn]=="numeric") "gaussian" else "binomial"
            pf <- summary(glm(ff,data=dd,family=fam))$coef
            pf <- pf[2:NROW(pf),4]
            pf <- format.pval(pf,digits=pdigits,eps=eps)
            ##           pf <- chisq.test(m[[nn]],m[[Fname]])$p.value
            ##           pf <- c(rep("--",length(levels(m[[nn]]))-1),signif(pf,digits=2))
            pf
          }))
          pVal
        }
      })
      controlPval <- unlist(controlPval)
      names(controlPval) <- theLevels
    }
  }

  # }}}
  # {{{ adding the group names
  
  outRowsL <- sapply(outL,function(x)ifelse(is.null(nrow(x)),1,NROW(x)))
  firstCol <- unlist(lapply(1:NL,function(i){
    c(theLevels[i],rep("",outRowsL[i]-1))
  }))

  pName <- if(pValues && match("ANOVA",pvalMethod,nomatch=FALSE)) "p-values" else NULL
  out <- data.frame(cbind(c(firstCol,pName),out),
                    stringsAsFactors=FALSE)
  names(out) <- c(Fname,"N",factorNames)
  if(sampleSize==FALSE) out <- out[,-2,drop=FALSE]
  rownames(out) <- 1:NROW(out)
  if (pValues && match("Control",pvalMethod,nomatch=FALSE)){
    out <- cbind(out,"p-value (vs control)"=controlPval)
    ## for (levelName in names(controlPval)){
    ## oldVal <- out[out[,Fname]==levelName,]
    ## newVal <- paste(oldVal,"; p=",c("","",controlPval[[levelName]]),sep="")
    ## out[out$Fname==levelName,] <- newVal
    ## }
  }
  # }}}
  class(out) <- c("crossTable","data.frame")
  if (transpose) {
    tout <- t(out[,-1])
    colnames(tout) <- paste(names(out)[1],out[,1],sep=":")
    out <- tout
  }
  out
}


