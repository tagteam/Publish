univariateTable <- function(formula,
                            data=parent.frame(),
                            summary.format="mean(x) (sd(x))",
                            freq.format="count(x) (percent(x))",
                            digits.summary=2,
                            digits.freq=1,
                            strataIsOutcome=FALSE,
                            preferOR=FALSE,
                            pvalue.nonpar=FALSE,
                            pvalue.eps=0.0001,
                            pvalue.digits=4,
                            pvalue.stars=FALSE,
                            sep="-",
                            ...){
  # {{{helper functions
  S <- function(x,format,digits)x
  F <- function(x,ref,digits)x
  iqr <- function(x)quantile(x,c(0.25,0.75))
  minmax <- function(x)quantile(x,c(0,1))
  # }}}
  # {{{ parse formula and find data
  formList <- readFormula(formula,specials=c("F","S"),specialArgumentNames=list("S"="format"),alias=list("strata"="F","factor"="F","C"="S"),unspecified="auto")
  call <- match.call()
  m <- match.call(expand = FALSE)
  if (match("subset",names(call),nomatch=FALSE))
    stop("Subsetting of data is not possible.")
  m <- m[match(c("","formula","data","subset","na.action"),names(m),nomatch = 0)]
  m[[1]]  <-  as.name("model.frame")
  m$formula <- formList$allVars
  m$na.action <- "na.pass"
  theData <- eval(m, parent.frame())
  # }}}
  # {{{ extract grouping variable
  if (is.null(formList$Response)){
    groupvar <- NULL
    groupname <- NULL
    grouplabels <- NULL
    groups <- NULL
  }
  else{
    groupvar <- as.character(model.response(model.frame(formula=formList$Response,data=theData,na.action="na.pass")))
    ## m <- model.frame(formula,data,na.action=na.pass)
    ## groupname <- names(m)[1]
    groupname <- all.vars(formList$Response)
    ## deal with missing values in group var
    ## groupvar <- as.character(m[,1])
    groupvar[is.na(groupvar)] <- "Missing"
    groupvar <- factor(groupvar)
    groups <- levels(groupvar)
    if (strataIsOutcome==TRUE & (length(groups)!=2))
      stop("strataIsOutcome can only be TRUE when there are exactly two groups. You have ",length(groups)," groups")
    ## if (length(groups)>30) stop("More than 30 groups")
    grouplabels <- paste(groupname,"=",groups)
  }
  # }}}
  # {{{ classify variables into continuous numerics and grouping factors
  unspecified <- all.vars(formList$auto$formula)
  automatrix <- model.frame(formula=formList$auto$formula,data=theData,na.action="na.pass")
  auto.type <- sapply(1:NCOL(automatrix),function(i){
    x <- automatrix[,i]
    #  type 0=character
    #       1=factor
    #       2=numeric
    ## set some useful default
    type.i <- is.factor(x)+2*is.numeric(x)+3*is.logical(x)
    # treat character and logical as factors
    if (type.i %in% c(0,3)) type.i <- 1
    # force variables with less than 3 distinct values to be factors (discrete)
    if (length(unique(x))<3) type.i <- 1
    type.i})
  if (any(auto.type==2)){
    if (length(formList$S$formula)>0)
      formList$S$formula <- update(formList$S$formula,paste("~.+",paste(unspecified[auto.type==2],collapse="+")))
    else
      formList$S$formula <- formula(paste("~",paste(unspecified[auto.type==2],collapse="+")))
  }
  if (any(auto.type==1)){
    if (length(formList$F$formula)>0)
      formList$F$formula <- update(formList$F$formula,paste("~.+",paste(unspecified[auto.type==1],collapse="+")))
    else
      formList$F$formula <- formula(paste("~",paste(unspecified[auto.type==1],collapse="+")))
  }
  if (is.null(formList$F$formula)){
    factor.matrix <- NULL
  }
  else{
    factor.matrix <- model.frame(formula=formList$F$formula,data=theData,na.action="na.pass")
  }
  if (is.null(formList$S$formula)){
    continuous.matrix <- NULL
  } else{
    continuous.matrix <- model.frame(formula=formList$S$formula,data=theData,,na.action="na.pass")
  }
  NVARS <- NCOL(continuous.matrix)+NCOL(factor.matrix)
  # }}}
  # {{{ summary numeric variables
  if (!is.null(continuous.matrix)){
    # prepare format
    sumformat <- parseSummaryFormat(format=summary.format,digits=digits.summary)
    #  get summary excluding missing in groups and in totals
    summaryNumeric <- getSummary(matrix=continuous.matrix,
                                 varnames=names(continuous.matrix),
                                 groupvar=groupvar,
                                 groups=groups,
                                 labels=grouplabels,
                                 stats=sumformat$stats,
                                 format=sumformat$format)
  }
  else{
    sumformat <- NULL
    summaryNumeric <- NULL
  }
  # }}}  
  # {{{ discrete variables (factors)
  if (!is.null(factor.matrix)){
    # prepare format
    freqformat <- parseFrequencyFormat(format=freq.format,digits=digits.freq)  
    #  get frequencies excluding missing in groups and in totals
    freqFactor <- getFrequency(matrix=factor.matrix,varnames=names(factor.matrix),groupvar=groupvar,groups=groups,labels=grouplabels,stats=freqformat$stats,format=freqformat$format)
  }
  else{
    freqformat <- NULL
    freqFactor <- NULL
  }
  # }}}
  # {{{ missing values
  if (is.null(continuous.matrix)){
    allmatrix <- factor.matrix
  }
  else{
    if (is.null(factor.matrix)){
      allmatrix <- continuous.matrix
    }
    else{
      allmatrix <- cbind(continuous.matrix,factor.matrix)
    }
  }
  totals.missing <- lapply(allmatrix,function(v){sum(is.na(v))})
  if (!is.null(groups)){
    group.missing <- lapply(allmatrix,function(v){
      lapply(groups,function(g){
        sum(is.na(v[groupvar==g]))
      })
    })}
  else {
    group.missing <- NULL
  }
  # }}}
  # {{{ p-values
  p.cont <- NULL
  p.freq <- NULL
  if (!is.null(groups)){
    if (!is.null(continuous.matrix)){    
      p.cont <- sapply(names(continuous.matrix),function(v){
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
    }
    if (!is.null(factor.matrix)){
      p.freq <- sapply(names(factor.matrix),function(v){
        if (strataIsOutcome==TRUE){
          format.pval(anova(glm(update(formula,paste(".~",v)),data=data,family=binomial),test="Chisq")$"Pr(>Chi)"[2],eps=pvalue.eps,digits=pvalue.digits)}
        else{
          tabx <- table(factor.matrix[,v],groupvar)
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
            test <- suppressWarnings(chisq.test(tabx))
            if (any(test$expected < 5) && is.finite(test$parameter))
              warning(paste("univariate table (chisq.test): Chi-squared approximation may be incorrect for",v),call.=FALSE)
            px <- format.pval(test$p.value,eps=pvalue.eps,digits=pvalue.digits)
            if (pvalue.stars==TRUE)
              px <- symnum(px,corr = FALSE,na = FALSE,cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),symbols = c("***", "**", "*", ".", " "))
            px
          }
        }
      })
    }
  }
  # }}}
  # {{{ output
  xlevels <- lapply(factor.matrix,function(x){
    ## levels(as.factor(x,exclude=FALSE))
    levels(as.factor(x))
  })
  vartypes <- rep(c("numeric","factor"),c(length(names(continuous.matrix)),length(names(factor.matrix))))
  names(vartypes) <- c(names(continuous.matrix),names(factor.matrix))
  out <- list(summary.groups=c(freqFactor$groupfreq,summaryNumeric$groupsummary),
              summary.totals=c(freqFactor$totals,summaryNumeric$totals),
              missing=list(group=group.missing,totals=totals.missing),
              p.values=c(p.cont,p.freq),
              formula=formula,
              groups=grouplabels,
              vartype=vartypes,
              xlevels=xlevels,
              statistics=sumformat$stats)
  class(out) <- "univariateTable"
  out
  # }}}
} 
