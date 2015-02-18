##' ##' ##' ##' Categorical variables are summarized using counts and frequencies.
##' Continuous variables are summarized by means and standard deviations.
##' Deviations from the above defaults are obtained when the 
##' arguments summary.format and freq.format are combined with suitable
##' summary functions. 
##' 
##' 
##' @title Univariate table
##' @param formula Formula specifying the grouping variable (strata) on the left hand side (can be omitted)
##' and on the right hand side the variables for which to obtain (descriptive) statistics.
##' @param data Data set in which formula is evaluated
##' @param summary.format Format for the numeric (non-factor) variables. Default is mean (SD).
##' If different formats are desired, either special Q can be used or the function is called multiple times
##' and the results are rbinded. See examples.
##' @param Q.format Format for quantile summary of numerice variables: Default is median (inter quartile range).
##' @param freq.format Format for categorical variables. Default is count (percentage).
##' @param column.percent Logical, if \code{TRUE} and the default freq.format is used then column percentages are given instead of row percentages for discrete factors.
##' @param digits.summary Rounding digits for summary.format.
##' @param digits.freq Rounding digits for freq.format.
##' @param strataIsOutcome If \code{TRUE} logistic regression is used instead of t-tests and Wilcoxon rank tests
##' to compare numeric variables across groups.
##' @param shortGroupNames If \code{TRUE} group names are abbreviated.
##' @param ... Not used (not yet)
##' @return List 
##' @author Thomas A. Gerds
##' @seealso summary.univariateTable, publish.univariateTable
##' @examples
##' data(Diabetes)
##' univariateTable(~age,data=Diabetes)
##' univariateTable(~gender,data=Diabetes)
##' univariateTable(~age+gender+ height+weight,data=Diabetes)
##' univariateTable(location~age+gender+height+weight,data=Diabetes)
##' 
##' ## Use quantiles and rank tests for some variables and mean and standard deviation for others
##' univariateTable(gender~Q(age)+location+Q(BMI)+height+weight,
##'                 data=Diabetes)
##' 
##' ## Factor with more than 2 levels
##' Diabetes$AgeGroups <- cut(Diabetes$age,
##'                           c(19,29,39,49,59,69,92),
##'                           include.lowest=TRUE)
##' univariateTable(location~AgeGroups+gender+height+weight,
##'                 data=Diabetes)
##' 
##' ## Column percent
##' univariateTable(location~gender+age+AgeGroups,
##'                 data=Diabetes,
##'                 column.percent=TRUE)
##' 
##' ## changing Labels
##' u <- univariateTable(location~gender+AgeGroups+ height + weight,
##'                      data=Diabetes,
##'                      column.percent=TRUE,
##'                      freq.format="count(x) (percent(x))")
##' summary(u,"AgeGroups"="Age (years)","height"="Height (inches)")
##' 
##' ## multiple summary formats
##' ## suppose we want for some reason mean (range) for age
##' ## and median (range) for BMI.
##' ## method 1:
##' univariateTable(frame~Q(age)+BMI,
##'                 data=na.omit(Diabetes),
##'                 Q.format="mean(x) (range(x))",
##'                 summary.format="median(x) (range(x))")
##' ## method 2: 
##' u1 <- summary(univariateTable(frame~age,
##'                               data=na.omit(Diabetes),
##'                               summary.format="mean(x) (range(x))"))
##' u2 <- summary(univariateTable(frame~BMI,
##'                               data=na.omit(Diabetes),
##'                               summary.format="median(x) (range(x))"))
##' publish(rbind(u1,u2),digits=2)
##'
##' @export
univariateTable <- function(formula,
                            data=parent.frame(),
                            summary.format="mean(x) (sd(x))",
                            Q.format="median(x) [iqr(x)]",
                            freq.format="count(x) (percent(x))",
                            column.percent=TRUE,
                            digits.summary=1,
                            digits.freq=1,
                            strataIsOutcome=FALSE,
                            shortGroupNames,
                            ...){

    call <- match.call()
    ## m <- match.call(expand.dots = FALSE)
    ## if (match("subset",names(call),nomatch=FALSE))
        ## stop("Subsetting of data is not possible.")
    ## m <- m[match(c("","formula","data","subset","na.action"),names(m),nomatch = 0)]
    ## m[[1]]  <-  as.name("model.frame")
    ## m$formula <- formula
    ## m$na.action <- "na.pass"
    ## theData <- eval(m, parent.frame())
    # {{{ parse formula and find data
    oldnaaction <- options()$na.action
    options(na.action="na.pass")
    FRAME <- specialFrame(formula,
                          data,
                          specialsDesign=FALSE,
                          unspecialsDesign=FALSE,
                          specials=c("F","S","Q"),
                          specialsFactor = FALSE,
                          stripSpecials=c("F","S","Q"),
                          stripArguments=list("S"="format"),
                          stripAlias=list("strata"="F","factor"="F","Cont"="S","nonpar"="Q"),
                          na.action="na.pass")
    options(na.action=oldnaaction)
    # }}}
    # {{{ extract grouping variable
    if (is.null(FRAME$response)){
        groupvar <- NULL
        groupname <- NULL
        grouplabels <- NULL
        groups <- NULL
        n.groups <- NROW(data)
    }
    else{
        ## mr <- model.response(model.frame(formula=formList$Response,data=theData,drop.unused.levels = TRUE,na.action="na.pass"))
        mr <- FRAME$response
        stopifnot(NCOL(mr)==1)
        groupname <- colnames(mr)
        groupvar <- as.character(FRAME$response[,1,drop=TRUE])
        ## deal with missing values in group var
        groupvar[is.na(groupvar)] <- "Missing"
        if (is.factor(mr))
            if (any(is.na(groupvar)))
                groups <- c(levels(mr),"Missing")
            else 
                groups <- levels(mr)
        else
            groups <- unique(groupvar)
        groupvar <- factor(groupvar,levels=groups)
        n.groups <- table(groupvar)
        n.groups <- c(n.groups,sum(n.groups))
        if (strataIsOutcome==TRUE & (length(groups)!=2))
            stop("strataIsOutcome can only be TRUE when there are exactly two groups. You have ",length(groups)," groups")
        ## if (length(groups)>30) stop("More than 30 groups")
        if (missing(shortGroupNames)){
            if(all(nchar(groups)<2) || all(groups %in% c(TRUE,FALSE)))
                shortGroupNames <- FALSE
            else
                shortGroupNames <- TRUE
        }
        if (shortGroupNames==TRUE)
            grouplabels <- groups
        else
            grouplabels <- paste(groupname,"=",groups)
    }
   
    # }}}
    # {{{ classify variables into continuous numerics and grouping factors
    automatrix <- FRAME$design
    continuous.matrix <- NULL
    factor.matrix <- NULL
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
        if (is.null(FRAME$S))
            continuous.matrix <- automatrix[,auto.type==2,drop=FALSE]
        else
            continuous.matrix <- cbind(FRAME$S,automatrix[,auto.type==2,drop=FALSE])
    }
    if (any(auto.type==1)){
        if (is.null(FRAME$F))        
            factor.matrix <- automatrix[,auto.type==1,drop=FALSE]
        else
            factor.matrix <- cbind(FRAME$F,automatrix[,auto.type==1,drop=FALSE])
    }
    Q.matrix <- FRAME$Q
    NVARS <- NCOL(continuous.matrix)+ NCOL(continuous.matrix)+NCOL(factor.matrix)+ NCOL(Q.matrix)
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
                                     format=sumformat$format,
                                     digits=digits.summary)
    }
    else{
        sumformat <- NULL
        summaryNumeric <- NULL
    }
    if (!is.null(Q.matrix)){
        # prepare format
        Qformat <- parseSummaryFormat(format=Q.format,digits=digits.summary)
        #  get summary excluding missing in groups and in totals
        qNumeric <- getSummary(matrix=Q.matrix,
                               varnames=names(Q.matrix),
                               groupvar=groupvar,
                               groups=groups,
                               labels=grouplabels,
                               stats=Qformat$stats,
                               format=Qformat$format)
    }
    else{
        Qformat <- NULL
        qNumeric <- NULL
    }
    # }}}  
    # {{{ discrete variables (factors)
    if (!is.null(factor.matrix)){
        if (column.percent==TRUE){
            freq.format <- sub("percent","colpercent",freq.format)
            freq.format <- sub("colcolpercent","colpercent",freq.format)
        }
        # prepare format
        freqformat <- parseFrequencyFormat(format=freq.format,digits=digits.freq)  
        #  get frequencies excluding missing in groups and in totals
        freqFactor <- getFrequency(matrix=factor.matrix,
                                   varnames=names(factor.matrix),
                                   groupvar=groupvar,
                                   groups=groups,
                                   labels=grouplabels,
                                   stats=freqformat$stats,
                                   format=freqformat$format)
    }
    else{
        freqformat <- NULL
        freqFactor <- NULL
    }
    # }}}
    # {{{ missing values
    mlist <- list(continuous.matrix,Q.matrix,factor.matrix)
    allmatrix <- do.call("cbind",mlist[!sapply(mlist,is.null)])
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
    p.Q <- NULL
    p.freq <- NULL
    if (!is.null(groups)){
        if (!is.null(continuous.matrix)){
            p.cont <- sapply(names(continuous.matrix),function(v){
                if (strataIsOutcome==TRUE){
                    ## logistic regression 
                    px <- anova(glm(update(formula,paste(".~",v)),data=data,family=binomial),test="Chisq")$"Pr(>Chi)"[2]
                    px
                }
                else {
                    px <- anova(glm(formula(paste(v,"~",groupname)),data=data),test="Chisq")$"Pr(>Chi)"[2]
                    px
                }
            })
        }
        if (!is.null(Q.matrix)){
            p.Q <- sapply(names(Q.matrix),function(v){
                if (strataIsOutcome==TRUE){
                    ## logistic regression 
                    px <- anova(glm(update(formula,paste(".~",v)),data=data,family=binomial),test="Chisq")$"Pr(>Chi)"[2]
                    px
                }
                else {
                    px <- kruskal.test(formula(paste(v,"~",groupname)),data=data)$p.value
                    px
                }
            })
        }
        if (!is.null(factor.matrix)){
            p.freq <- sapply(names(factor.matrix),function(v){
                if (strataIsOutcome==TRUE){
                    ## logistic regression 
                    ## format.pval(
                    ## warning(v)
                    px <- anova(glm(update(formula,paste(".~",v)),data=data,family=binomial),test="Chisq")$"Pr(>Chi)"[2]
                       ## ,eps=pvalue.eps,digits=pvalue.digits)}
                } else{
                    tabx <- table(factor.matrix[,v],groupvar)
                    if (sum(tabx)==0) {
                        px <- NA
                    } else{
                        suppressWarnings(test <- chisq.test(tabx))
                        px <- test$p.value
                    }
                    ## FIXME: need to catch and pass the warnings 
                    ## test <- suppressWarnings(fisher.test(tabx))
                    ## if (any(test$expected < 5) && is.finite(test$parameter))
                    px
                }
            })
        }
    }
    # }}}
    # {{{ output
    ## xlevels <- lapply(factor.matrix,function(x){
        ## levels(as.factor(x,exclude=FALSE))
        ## levels(as.factor(x))
    ## })
    vartypes <- rep(c("numeric","Q","factor"),c(length(names(continuous.matrix)),length(names(Q.matrix)),length(names(factor.matrix))))
    names(vartypes) <- c(names(continuous.matrix),names(Q.matrix),names(factor.matrix))
    out <- list(summary.groups=c(freqFactor$groupfreq,summaryNumeric$groupsummary,qNumeric$groupsummary),
                summary.totals=c(freqFactor$totals,summaryNumeric$totals,qNumeric$totals),
                missing=list(group=group.missing,totals=totals.missing),
                n.groups=n.groups,
                p.values=c(p.cont,p.Q,p.freq),
                formula=formula,
                groups=grouplabels,
                vartype=vartypes,
                xlevels=freqFactor$xlevels,
                Q.format=Q.format,
                summary.format=summary.format,
                freq.format=freq.format)
    class(out) <- "univariateTable"
    out
    # }}}
} 
