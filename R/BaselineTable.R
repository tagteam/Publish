BaselineTable <- function(formula,
                          data=parent.frame(),
                          type,
                          summary.continuous="mean(x) (sd(x))",
                          summary.discrete="count(x) (percent(x))",
                          format.numeric="%1.2f",
                          strataIsOutcome=FALSE,
                          preferOR=FALSE,
                          pvalue.nonpar=FALSE,
                          pvalue.eps=0.0001,
                          pvalue.digits=4,
                          pvalue.stars=FALSE,
                          sep="-",
                          ...){

  # {{{helper functions
  iqr <- function(x)quantile(x,c(0.25,0.75))
  minmax <- function(x)quantile(x,c(0,1))
  # }}}
  # {{{ parse data 
  m <- model.frame(formula,data,na.action=na.pass)
  groupname <- names(m)[1]
  ## deal with missing values in group var
  groupvar <- as.character(m[,1])
  groupvar[is.na(groupvar)] <- "Missing"
  groupvar <- factor(groupvar)
  groups <- levels(groupvar)
  if (strataIsOutcome==TRUE & (length(groups)!=2))
    stop("strataIsOutcome can only be TRUE when there are exactly two groups. You have ",length(groups)," groups")
  grouplabels <- paste(names(m)[1],"=",groups)
  variables <- m[,-1,drop=FALSE]
  NVARS <- length(variables)
  if (missing(type)) type <- sapply(variables,function(x){is.factor(x)+2*is.numeric(x)+3*is.logical(x)})
  # treat logical as factors
  ## print(type)
  type[type==3] <- 1
  ## force variables with less than 3 distinc values to be discrete
  type[sapply(variables,function(v)length(unique(v)))<3] <- 1
  # }}}
  #  type 0=character
  #       1=factor
  #       2=numeric
  #       3=logical
  # {{{ summary numeric variables
  # {{{ prepare format
  tmp.cont <- strsplit(summary.continuous,"[ \t]+|\\(|\\{|\\[|\\)",perl=TRUE)[[1]]
  stats.cont <- tmp.cont[grep("^x$",tmp.cont)-1]
  outclass.cont <- sapply(stats.cont,function(s)class(do.call(s,list(1:2))))
  outlen.cont <- sapply(stats.cont,function(s)length(do.call(s,list(1:2))))
  for(s in 1:length(stats.cont)){
    subs <- format.numeric
    if(!(outlen.cont[s]%in%c(1,2)))
      stop(paste("The function",stats.cont[s],"returns",outlen.cont[s],"values (can be 1 or 2)"))
    subs <- switch(as.character(outlen.cont[s]),
                   "1"={switch(outclass.cont[s],"numeric"=format.numeric,"%s")},
                   "2"={switch(outclass.cont[s],"numeric"=paste(format.numeric,sep,format.numeric,sep=""),paste("%s",sep,"%s",sep=""))})
    summary.continuous <- gsub(paste(stats.cont[s],"(x)",sep=""),subs,summary.continuous,fixed=TRUE)
  }
  # }}}
  # {{{ get summary excluding missing in groups and in totals
  type.cont <- type[type==2]
  groups.cont <- vector(sum(type==2),mode="list")
  names(groups.cont) <- names(variables[,type==2,drop=FALSE])
  totals.cont <- vector(sum(type==2),mode="list")
  names(totals.cont) <- names(variables[,type==2,drop=FALSE])
  for (v in names(type.cont)){
    vv <- variables[,v]
    missing.v <- is.na(vv)
    vvv <- vv[!missing.v]
    ggg <- factor(groupvar[!missing.v],levels=levels(groupvar))
    totals.values <- lapply(stats.cont,function(s){
      do.call(s,list(vvv))
    })
    totals.cont[[v]] <- do.call("sprintf",c(summary.continuous,as.list(unlist(totals.values))))
    summary.group.cont <- lapply(groups,function(g){
      values <- lapply(stats.cont,function(s){
        do.call(s,list(vvv[ggg==g]))
      })
      do.call("sprintf",c(summary.continuous,as.list(unlist(values))))
    })
    names(summary.group.cont) <- grouplabels
    groups.cont[[v]] <- do.call("cbind", summary.group.cont)
  }
  ## })
  ## names(out.cont) <- names(variables[,type==2,drop=FALSE])
  # }}}  
  # }}}
  # {{{ discrete variables (factors)
  # {{{ prepare format 
  tmp.disc <- strsplit(summary.discrete,"[ \t]+|\\(|\\{|\\[|\\)",perl=TRUE)[[1]]
  stats.disc <- tmp.disc[grep("^x$",tmp.disc)-1]
  for(s in 1:length(stats.disc)){
    subs <- switch(stats.disc[s],"count"="%d","percent"="%1.1f", "colpercent"="%1.1f",
                   stop("Can only do count and percent for discrete variables"))
    summary.discrete <- gsub(paste(stats.disc[s],"(x)",sep=""),subs,summary.discrete,fixed=TRUE)
  }
  # }}}
  # {{{ get levels
  type.disc <- type[type==1]
  xlevels <- lapply(variables[type==1],function(x){
    ## levels(as.factor(x,exclude=FALSE))
    levels(as.factor(x))
  })
  # }}}
  # {{{ get tables
  groups.disc <- vector(sum(type==1),mode="list")
  names(groups.disc) <- names(variables[,type==1,drop=FALSE])
  totals.disc <- vector(sum(type==1),mode="list")
  names(totals.disc) <- names(variables[,type==1,drop=FALSE])
  for (v in names(type.disc)){
    vv <- variables[,v]
    missing.v <- is.na(vv)
    vvv <- as.factor(vv[!missing.v])
    ggg <- factor(groupvar[!missing.v],levels=levels(groupvar))
    levels.x <- levels(vvv)
    totals.disc[[v]] <- table(vvv)
    tables <- lapply(split(ggg,vvv),function(x){
      xtab <- data.frame(table(factor(x,levels=groups)))
      if (match("percent",stats.disc,nomatch=FALSE)){
        xtab$Percent <- 100*xtab$Freq/sum(xtab$Freq)
      }
      tab.out <- lapply(1:NROW(xtab),function(row){
        values <- xtab[row,-1]
        if (match("colpercent",stats.disc,nomatch=FALSE)){
          values
        }
        else{
          do.call("sprintf",c(summary.discrete,as.list(unlist(values))))
        }
      })
      names(tab.out) <- grouplabels
      tab.out
    })
    groups.disc[[v]] <- do.call("rbind",tables)
    if (match("colpercent",stats.disc,nomatch=FALSE)){
      groups.disc[[v]] <- apply(groups.disc[[v]],2,function(x){
        val <- as.numeric(x)
        colp <- 100*val/sum(val)
        sapply(1:length(val),function(i){
          do.call("sprintf",c(summary.discrete,as.list(c(val[i],colp[i]))))
        })
      })
    }
  }
  # }}}
  # {{{ missing values
  totals.missing <- lapply(variables,function(v){sum(is.na(v))})
  group.missing <- lapply(variables,function(v){
    lapply(groups,function(g){
      sum(is.na(v[groupvar==g]))
    })
  })
  # }}}
  # {{{ p-values
  p.cont <- sapply(names(type.cont),function(v){
    if (strataIsOutcome==TRUE){
      px <- anova(glm(update(formula,paste(".~",v)),data=data,family=binomial),test="Chisq")$"Pr(>Chi)"[2]
      if (pvalue.stars==TRUE)
        px <- symnum(px,corr = FALSE,na = FALSE,cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),symbols = c("***", "**", "*", ".", " "))
      else
        px <- format.pval(px,eps=pvalue.eps,digits=pvalue.digits)
      px
    }
    else {
      if (pvalue.nonpar==TRUE){
        px <- kruskal.test(formula(paste(v,"~",groupname)),data=data)$p.value
        if (pvalue.stars==TRUE)
          px <- symnum(px,corr = FALSE,na = FALSE,cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),symbols = c("***", "**", "*", ".", " "))
        else
          px <- format.pval(px,eps=pvalue.eps,digits=pvalue.digits)
        px
      } else {
        px <- anova(glm(formula(paste(v,"~",groupname)),data=data),test="Chisq")$"Pr(>Chi)"[2]
        if (pvalue.stars==TRUE)
          px <- symnum(px,corr = FALSE,na = FALSE,cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),symbols = c("***", "**", "*", ".", " "))
        else
          px <- format.pval(px,eps=pvalue.eps,digits=pvalue.digits)
        px
      }
    }
  })
  p.disc <- sapply(names(type.disc),function(v){
    if (strataIsOutcome==TRUE){
      format.pval(anova(glm(update(formula,paste(".~",v)),data=data,family=binomial),test="Chisq")$"Pr(>Chi)"[2],eps=pvalue.eps,digits=pvalue.digits)}
    else{
      tabx <- table(variables[,v],groupvar)
      if (preferOR==TRUE){
        if (all(dim(tabx)==2)){
          orx <- oddsRatio2x2(tabx)
          ux <- paste(format.numeric, " [", format.numeric," - ", format.numeric,"] %s",sep="")
          if (pvalue.stars==TRUE)
            px <- symnum(orx$pvalue,corr = FALSE,na = FALSE,cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),symbols = c("***", "**", "*", ".", " "))
          else
            px <- orx$pvalue
          sprintf(ux, orx$or,orx$lower,orx$upper,px)
        } else{
          orx <- oddsRatioNx2(tabx)
          ux <- paste(format.numeric, " [", format.numeric," - ", format.numeric,"] %s",sep="")
          orx.tab <- do.call("rbind",lapply(orx,function(o){
            if (pvalue.stars==TRUE)
              if (is.na(o$pvalue))
                p <- ""
              else
                p <- symnum(o$pvalue,corr = FALSE,na = FALSE,cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),symbols = c("***", "**", "*", ".", " "))
            else
              p <- o$pvalue
            sprintf(ux, o$or,o$lower,o$upper,p)
          }))
          orx.tab[1,] <- gsub("NA","",orx.tab[1,])
          orx.tab
        }
      }
      else{
        px <- format.pval(chisq.test(tabx)$p.value,eps=pvalue.eps,digits=pvalue.digits)
        if (pvalue.stars==TRUE)
          px <- symnum(px,corr = FALSE,na = FALSE,cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),symbols = c("***", "**", "*", ".", " "))
        px
      }
    }
  })
  # }}}
  out <- list(summary.groups=c(groups.disc,groups.cont),summary.totals=c(totals.cont,totals.disc),missing=list(group=group.missing,totals=totals.missing),p.values=c(p.cont,p.disc),formula=formula,groups=grouplabels,vartype=type,xlevels=xlevels,statistics=stats.cont)
  class(out) <- "BaselineTable"
  out
  # }}}
} 
