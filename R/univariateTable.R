##' Categorical variables are summarized using counts and frequencies.
##'
##' This function can generate the baseline demographic characteristics
##' that forms table 1 in many publications. It is also useful for generating
##' other tables of univariate statistics.
##'
##' The result of the function is an object (list) which containe the various data
##' generated. In most applications the \code{summary} function should be applied which generates
##' a data.frame with a (nearly) publication ready table. Standard manipulation can be
##' used to modify, add or remove columns/rows and for users not accustomed to R the table
##' generated can be exported to a text file which can be read by other software, e.g., via
##' write.csv(table,file="path/to/results/table.csv")
##'
##' Continuous variables are summarized by means and standard deviations.
##' Deviations from the above defaults are obtained when the
##' arguments summary.format and freq.format are combined with suitable
##' summary functions.
##'
##' @title Univariate table
##' @aliases utable univariateTable
##' @param formula Formula specifying the grouping variable (strata)
##' on the left hand side (can be omitted) and on the right hand side
##' the variables for which to obtain (descriptive) statistics.
##' @param data Data set in which formula is evaluated
##' @param summary.format Format for the numeric (non-factor)
##' variables. Default is mean (SD).  If different formats are
##' desired, either special Q can be used or the function is called
##' multiple times and the results are rbinded. See examples.
##' @param Q.format Format for quantile summary of numerical
##' variables: Default is median (inter quartile range).
##' @param freq.format Format for categorical variables. Default is
##' count (percentage).
##' @param column.percent Logical, if \code{TRUE} and the default
##' freq.format is used then column percentages are given instead of
##' row percentages for categorical variables (factors).
##' @param digits Number of digits
##' @param big.mark For formatting large numbers (i.e., greater than 1,000). \code{""} turn this off.  
##' @param short.groupnames If \code{TRUE} group names are abbreviated.
##' @param compare.groups Method used to compare groups. If
##' \code{"logistic"} and there are exactly two groups logistic
##' regression is used instead of t-tests and Wilcoxon rank tests to
##' compare numeric variables across groups.
##' @param show.totals If \code{TRUE} show a column with totals.
##' @param n If \code{TRUE} show the number of subjects as a separate
##' row.  If equal to \code{"inNames"}, show the numbers in
##' parentheses in the column names. If \code{FALSE} do not show
##' number of subjects.
##' @param outcome Outcome data used to calculate p-values when
##' compare groups method is \code{'logistic'} or \code{'cox'}.
##' @param na.rm If \code{TRUE} remove missing values from categorical
##' variables when calculating p-values.
##' @param ... saved as part of the result to be passed on to
##' \code{labelUnits}
##' @return List with one summary table element for each variable on the right hand side of formula.
##' The summary tables can be combined with \code{rbind}. The function \code{summary.univariateTable}
##' combines the tables, and shows p-values in custom format.
##' @author Thomas A. Gerds
##' @seealso summary.univariateTable, publish.univariateTable
##' @examples
##' data(Diabetes)
##' library(data.table)
##' univariateTable(~age,data=Diabetes)
##' univariateTable(~gender,data=Diabetes)
##' univariateTable(~age+gender+ height+weight,data=Diabetes)
##' ## same thing but less typing
##' utable(~age+gender+ height+weight,data=Diabetes)
##'
##' ## summary by location: 
##' univariateTable(location~Q(age)+gender+height+weight,data=Diabetes)
##' ## continuous variables marked with Q() are (by default) summarized
##' ## with median (IQR) and kruskal.test (with two groups equivalent to wilcox.test)
##' ## variables not marked with Q() are (by default) summarized
##' ## with mean (sd) and anova.glm(...,test="Chisq")
##' ## the p-value of anova.glm with only two groups is similar
##' ## but not exactly equal to that of a t.test
##' ## categorical variables are (by default) summarized by count
##' ## (percent) and anova.glm(...,family=binomial,test="Chisq")
##'
##' ## export result to csv
##' table1 = summary(univariateTable(location~age+gender+height+weight,data=Diabetes),
##' show.pvalues=FALSE)
##' # write.csv(table1,file="~/table1.csv",rownames=FALSE)
##'
##' ## change labels and values
##' utable(location~age+gender+height+weight,data=Diabetes,
##'        age="Age (years)",gender="Sex",
##'        gender.female="Female",
##'        gender.male="Male",
##'        height="Body height (inches)",
##'        weight="Body weight (pounds)")
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
##' ## Row percent
##' univariateTable(location~gender+age+AgeGroups,
##'                 data=Diabetes,
##'                 column.percent=FALSE)
##' 
##' ## change of frequency format
##' univariateTable(location~gender+age+AgeGroups,
##'                 data=Diabetes,
##'                 column.percent=FALSE,
##'                 freq.format="percent(x) (n=count(x))")
##'
##' ## changing Labels
##' u <- univariateTable(location~gender+AgeGroups+ height + weight,
##'                      data=Diabetes,
##'                      column.percent=TRUE,
##'                      freq.format="count(x) (percent(x))")
##' summary(u,"AgeGroups"="Age (years)","height"="Height (inches)")
##'
##' ## more than two groups
##' Diabetes$frame=factor(Diabetes$frame,levels=c("small","medium","large"))
##' univariateTable(frame~gender+BMI+age,data=Diabetes)
##'
##' Diabetes$sex=as.numeric(Diabetes$gender)
##' univariateTable(frame~sex+gender+BMI+age,
##'                 data=Diabetes,freq.format="count(x) (percent(x))")
##'
##' ## multiple summary formats
##' ## suppose we want for some reason mean (range) for age
##' ## and median (range) for BMI.
##' ## method 1:
##' univariateTable(frame~Q(age)+BMI,
##'                 data=Diabetes,
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
##' ## Large number format (big.mark)
##' Diabetes$AGE <- 1000*Diabetes$age
##' u3 <- summary(univariateTable(frame~AGE,
##'                               data=Diabetes,big.mark="'"))
##' 
##' 
##'
##' @export
univariateTable <- function(formula,
                            data=parent.frame(),
                            summary.format="mean(x) (sd(x))",
                            Q.format="median(x) [iqr(x)]",
                            freq.format="count(x) (percent(x))",
                            column.percent=TRUE,
                            digits=c(1,1,3),
                            big.mark=",",
                            short.groupnames,
                            compare.groups=TRUE,
                            show.totals=TRUE,
                            n="inNames",
                            outcome=NULL,
                            na.rm=FALSE,
                            ...){
    if (length(digits)<3) digits <- rep(digits,3)
    if (!is.numeric(digits.summary <- digits[[1]])) digits.summary <- 1
    if (!is.numeric(digits.freq <- digits[[2]])) digits.freq <- 1
    if (!is.numeric(pvalue.digits <- digits[[3]])) pvalue.digits <- 3
    call <- match.call()
    # {{{ parse formula and find data
    oldnaaction <- options()$na.action
    options(na.action="na.pass")
    FRAME <- specialFrame(formula,
                          data,
                          specials.design=FALSE,
                          unspecials.design=FALSE,
                          specials=c("F","S","Q","strata","Strata","factor","Factor","Cont","nonpar"),
                          specials.factor = FALSE,
                          strip.specials=c("F","S","Q"),
                           strip.arguments=list("S"="format"),
                           strip.alias=list("F"=c("strata","factor","Strata","Factor"),"S"="Cont","Q"="nonpar"),
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
        mr <- FRAME$response
        if(NCOL(mr)!=1) stop("Can only handle univariate outcome")
        groupname <- colnames(mr)
        groupvar <- as.character(FRAME$response[,1,drop=TRUE])
        mr <- FRAME$response[,1,drop=TRUE]
        ## deal with missing values in group variable
        if (is.factor(mr)){
            if (any(is.na(groupvar))){
                groupvar[is.na(groupvar)] <- "Missing"
                groups <- c(levels(mr),"Missing")
            }else{
                groups <- levels(mr)
            }
        } else {
            if (any(is.na(groupvar))){
                groupvar[is.na(groupvar)] <- "Missing"
            }
            groups <- unique(groupvar)
        }
        groupvar <- factor(groupvar,levels=groups)
        n.groups <- table(groupvar)
        n.groups <- c(n.groups,sum(n.groups))
        if (compare.groups=="logistic" & (length(groups)!=2))
            stop("compare.groups can only be equal to 'logistic' when there are exactly two groups. You have ",length(groups)," groups")
        ## if (length(groups)>30) stop("More than 30 groups")
        if (missing(short.groupnames)){
            if(all(nchar(groups)<2) || all(groups %in% c(TRUE,FALSE)))
                short.groupnames <- FALSE
            else
                short.groupnames <- TRUE
        }
        if (short.groupnames==TRUE)
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
        #  type 0=other coerced to numeric
        #       1=factor
        #       2=numeric
        #       3=character
        ## set some useful default
        type.i <- is.factor(x)+2*is.numeric(x)+3*is.logical(x)+4*is.character(x)
        # treat character and logical as factors
        if (type.i %in% c(3,4)) type.i <- 1
        # treat other variables as numeric (e.g. difftime)
        if (type.i==0) type.i <- 2
        # force variables with less than 3 distinct values to be categorical (factors) 
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
                                     digits=digits.summary,big.mark=big.mark)
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
                               format=Qformat$format,digits=digits.summary,big.mark=big.mark)
    }
    else{
        Qformat <- NULL
        qNumeric <- NULL
    }
    # }}}
    # {{{ categorical variables (factors)
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
                                   format=freqformat$format,big.mark=big.mark,digits=digits.freq)
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
    if (!is.null(groups) && (compare.groups!=FALSE)){
        if (!is.null(continuous.matrix)){
            p.cont <- sapply(names(continuous.matrix),function(v){
                data.table::set(data,j=v,value=as.numeric(data[[v]]))
                switch(tolower(as.character(compare.groups[[1]])),
                       "false"={NULL},
                       "logistic"={
                           ## logistic regression
                           px <- anova(glm(update(formula,paste(".~",v)),data=data,family=binomial),test="Chisq")$"Pr(>Chi)"[2]
                           px
                       },
                       "cox"={
                           px <- anova(coxph(formula(paste("Surv(time,status)~",v)),data=cbind(outcome,data)))$"Pr(>|Chi|)"[2]
                           px
                       },
                       "true"={
                           ## glm fails when there are missing values
                           ## in outcome, so we remove missing values
                           fv <- formula(paste(v,"~",groupname))
                           vdata <- model.frame(fv,data,na.action=na.omit)
                           px <- anova(glm(fv,data=vdata),test="Chisq")$"Pr(>Chi)"[2]
                           px
                       },NULL)
            })
        }
        if (!is.null(Q.matrix)){
            p.Q <- sapply(names(Q.matrix),function(v){
                switch(tolower(as.character(compare.groups[[1]])),
                       "false"={NULL},
                       "logistic"={
                           ## logistic regression
                           ## glm fails when there are missing values
                           ## in outcome, so we remove missing values
                           fv <- formula(paste(v,"~",groupname))
                           vdata <- model.frame(fv,data,na.action=na.omit)
                           px <- anova(glm(update(formula,paste(".~",v)),data=vdata,family=binomial),test="Chisq")$"Pr(>Chi)"[2]
                           px
                       },
                       "cox"={
                           px <- anova(coxph(formula(paste("Surv(time,status)~",v)),data=cbind(outcome,data)))$"Pr(>|Chi|)"[2]
                           px
                       },
                       "true"={
                           if (is.character(data[[groupname]])){
                               data[[paste0(groupname,"asfactor")]] <- factor(data[[groupname]])
                               px <- kruskal.test(formula(paste0(v,"~",groupname,"asfactor")),data=data)$p.value
                           } else{
                               px <- kruskal.test(formula(paste(v,"~",groupname)),data=data)$p.value
                           }
                           px
                       },NULL)
            })
        }
        if (!is.null(factor.matrix)){
            p.freq <- sapply(names(factor.matrix),function(v){
                switch(tolower(as.character(compare.groups[[1]])),
                       "false"={NULL},
                       "logistic"={
                           ## logistic regression
                           fv <- formula(paste(v,"~",groupname))
                           vdata <- model.frame(fv,data,na.action=na.omit)
                           px <- anova(glm(update(formula,paste(".~",v)),data=vdata,family=binomial),test="Chisq")$"Pr(>Chi)"[2]
                       },
                       "cox"={
                           px <- anova(coxph(formula(paste("Surv(time,status)~",v)),data=cbind(outcome,data)))$"Pr(>|Chi|)"[2]
                           px
                       },
                       "true"={
                           fv <- factor.matrix[,v]
                           missv <- is.na(fv)
                           if (any(missv) & na.rm==TRUE){
                               fv <- factor(fv[!missv])
                               groupvar <- factor(groupvar[!missv])
                           }
                           tabx <- table(fv,groupvar)
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
                       },NULL)
            })
        }
    }
    p.values <- c(p.cont,p.Q,p.freq)
    if (length(p.values)>0)
        if (is.null(p.values[[1]]))
            p.values <- NULL
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
                p.values=p.values,
                formula=formula,
                groups=grouplabels,
                vartype=vartypes,
                xlevels=freqFactor$xlevels,
                Q.format=Q.format,
                summary.format=summary.format,
                freq.format=freq.format,
                compare.groups=compare.groups,
                ## dots are passed to labelUnits without suitability checks
                show.totals=show.totals,
                n=n,
                labels=list(...))
    class(out) <- "univariateTable"
    out
    # }}}
}

## the name utable is more handy
##' @export utable
utable <- univariateTable
