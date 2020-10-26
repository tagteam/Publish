#' @title Subgroup Analysis - Interactions and estimates
#' @description
#'
#' The function can examine Cox regression, logistic regression 
#' and Poisson regression (Poisson regression for survival analysis)
#' where the effect of one variable is of particular interest. This function
#' systematically checks for effect modification with a list of other variables.
#' 
#' In randomised studies the main regression analysis is often univariate and
#' includes only the exposure of interest. In
#' observational studies the main regression analysis can readily be adjusted for
#' other variables including those which may modify the effect of the variable of interest.
#' 
#' @author Christian Torp-Pedersen
#' @usage
#' subgroupAnalysis(object,data,treatment,
#'    subgroups, confint.method="default",factor.reference="extraline")
#' @param object - glm, coxph or cph object for which subgroups should be
#' analyzed.
#' @param data - Dataset including all relevant variables
#' @param treatment - The variable to be examined in subgroups, best coded
#'   as 0/1 numeric.
#' @param subgroups - A vector of variable names presenting the variables
#' where subgroups should be formed. These variables should
#' all be "factors"
#' @param confint.method "default" creates Wald type confidence interval, "robust",
#' creates creates robust standard errors - see regressionTable function.
#' @param factor.reference "extraline" creates an extraline for the reference,
#' "inline" avoids this line.
#' @details 
#' The function can only handle a bivariate treatment, most conviniently coded as
#' zero or one. The p-value for interaction is obtained with a likelihood ratio test
#' comparing the main regression analysis with the interaction model. 
#' @return A data.frame with subsgroup specifications, number in each subgroup,
#' parameter estimates and p-value for interaction.  A forest plot
#' can be obtained with "plotConfidence".
#' @seealso coxph, glm, plotConfidence
#' @export
#' @examples
#' #load libraries
#' library(data.table)
#' library(Publish)
#' library(survival)
#' data(traceR) #get dataframe traceR
#' data.table::setDT(traceR)
#' traceR[,':='(wmi2=factor(wallMotionIndex<0.9,levels=c(TRUE,FALSE), 
#'                 labels=c("bad","good")),
#'              abd2=factor(abdominalCircumference<95, levels=c(TRUE,FALSE), 
#'                 labels=c("slim","fat")))]
#' traceR[,sex:=as.factor(sex)] # all subgroup variables needs to be factor                
#' traceR[observationTime==0,observationTime:=1]
#' # remove missing covariate values
#' traceR=na.omit(traceR)
#' # univariate analysis of smoking in subgroups of age and sex
#' # Main regression analysis is a simple/univariate Cox regression
#' fit_cox <- coxph(Surv(observationTime,dead)~treatment,data=traceR)
#' sub_cox <- subgroupAnalysis(fit_cox,traceR,treatment="treatment", 
#'   subgroups=c("smoking","sex","wmi2","abd2"))
#' sub_cox
#' 
#' # to see how the results are obtained consider the variable: smoking
#' fit_cox_smoke <- coxph(Surv(observationTime,dead)~treatment*smoking,data=traceR)
#' # the last three rows of the following output:
#' publish(fit_cox_smoke)
#' # are included in the first 3 rows of the result of the sub group analysis:
#' sub_cox[1:3,]
#' # the p-value is obtained as:
#' fit_cox_smoke_add <- coxph(Surv(observationTime,dead)~treatment+smoking,data=traceR)
#' anova(fit_cox_smoke_add,fit_cox_smoke,test="Chisq")
#'
#' # Note that a real subgroup analysis would be to subset the data
#' fit_cox1a <- coxph(Surv(observationTime,dead)~treatment,data=traceR[smoking=="never"])
#' fit_cox1b <- coxph(Surv(observationTime,dead)~treatment,data=traceR[smoking=="current"])
#' fit_cox1c <- coxph(Surv(observationTime,dead)~treatment,data=traceR[smoking=="prior"])
#'
#'
#' ## when the main analysis is already adjusted 
#' fit_cox_adj <- coxph(Surv(observationTime,dead)~treatment+smoking+sex+wmi2+abd2,
#'                  data=traceR)
#' sub_cox_adj <- subgroupAnalysis(fit_cox_adj,traceR,treatment="treatment",
#'   subgroups=c("smoking","sex","wmi2","abd2")) # subgroups as character string
#' sub_cox_adj
#' 
#' # When both start and end are in the Surv statement:
#' traceR[,null:=0]
#' fit_cox2 <- coxph(Surv(null,observationTime,dead)~treatment+smoking+sex+wmi2+abd2,data=traceR)
#' summary(regressionTable(fit_cox))
#' sub_cox2 <- subgroupAnalysis(fit_cox2,traceR,treatment="treatment",
#'   subgroups=c("smoking","sex","wmi2","abd2")) 
#' # Analysis with Poisson - and the unrealistic assumption of constant hazard
#' # and adjusted for age in all subgroups
#' fit_p <- glm(dead~treatment+age+offset(log(observationTime)),family="poisson",
#'            data=traceR)
#' sub_pois <- subgroupAnalysis(fit_p,traceR,treatment="treatment",
#'   subgroups=~smoking+sex+wmi2+abd2) 
#' # Analysis with logistic regression - and very wrongly ignoring censoring
#' fit_log <- glm(dead~treatment+age,family="binomial",data=traceR)
#' sub_log <- subgroupAnalysis(fit_log,traceR,treatment="treatment",
#'    subgroups=~smoking+sex+wmi2+abd2, factor.reference="inline")
subgroupAnalysis <- function(object, # glm, lrm, coxph or cph object
                             data, # data with all variables
                             treatment, # max 2 values
                             subgroups, # Character vector or Formula. Factor list of subgroups variables
                             confint.method="default", # Wald type confidence interval
                             factor.reference="extraline"){
  level=tail=Variable=NULL
  if(!(class(object)[1] %in% c("coxph","cph","glm"))) stop ("Error - Object must be coxph, cph or glm")
  if(!(class(treatment)[1]=="character")) stop("Error - Variable treament must be character")
  if(class(subgroups)[1]=="formula") subgroups <- all.vars(subgroups)
  else if(!(class(subgroups)[1]=="character")) stop ("Error - subgroups must be formula or character")
  if (!(class(data)[1] %in% c("data.frame","data.table"))) stop ("Error - data must be data.frame og data.table")
  else{
    datt <- copy(data)
    data.table::setDT(datt)
  }
  ## if (!all(stats::complete.cases(data[,.SD,.SDcols=c(subgroups,all.vars(object$formula),treatment)]))) 
    ## warning("data has missing values in columns used, may cause problems")
  if (!treatment %in% all.vars(object$formula)) stop("Error - treatment must be in the formula")
  #Define type of analysis
  if (class(object)[1] %in% c("coxph","cph")) model<-"cox"
  else if (class(object)[1]=="glm"){
    if(object$family$family=="binomial") model<-"logistic"
    else if (object$family$family=="poisson") model<-"poisson" # Poisson no offset
    else stop("Error - type of study not an option or misspecified")
  }
  #subgroups variables should not be in the models
  ## for (i in all.vars(object$formula)) if (i %in% subgroups)
    ## stop("Subgroups variables should not be part of every model")
  Result <- rbindlist(lapply(subgroups,function(var){
    ff1 <- update.formula(object$formula, paste("~ . +",var, "*", treatment)) #with interaction
    ff2 <- update.formula(object$formula, paste("~ . +",var, "+", treatment)) #without interaction
    if (model=='cox'){
      fit1 <- coxph(ff1,data=datt)
      fit2 <- coxph(ff2,data=datt)
      pinteraction <- anova(fit1,fit2)[4][2,]
      lhs <- all.vars(object$formula[[2]])
      if (length(lhs)==2){ # time fixed model
        eventtime <- datt[,list(sample=.N,
                                event=sum(eval(parse(text=lhs[2])),na.rm=TRUE),
                                time=sum(eval(parse(text=lhs[1])),na.rm=TRUE)),
                          by=c(var,treatment)]
        
      }
      else{ # Time varying model
          eventtime <- datt[,list(sample=.N,
                                  event=sum(eval(parse(text=lhs[3])),na.rm=TRUE),
                                  time=sum(eval(parse(text=lhs[2]))-eval(parse(text=lhs[1])),na.rm=TRUE)),
                            by=c(var,treatment)]
      }
      eventtime <- data.table::dcast(eventtime,paste(var,"~",treatment),
                                     value.var=list("sample","event","time"))
    }
    else if(model=="poisson"){
        if (!is.null(object$offset)){
            fit1 <- glm(ff1,family="poisson",data=datt)
            fit2 <- glm(ff2,family="poisson",data=datt)
            tt1 <- terms(ff1)
            timevar <- all.vars(ff1)[[attributes(tt1)$offset]]
            eventtime <- datt[,list(sample=.N,
                                    event=sum(eval(parse(text=all.vars(object$formula)[[1]])),na.rm=TRUE),
                                    time=sum(eval(parse(text=timevar))),na.rm=TRUE),
                              by=c(var,treatment)]
            eventtime <- data.table::dcast(eventtime,paste(var,"~",treatment),
                                           value.var=list("sample","event","time")) 
        }
        else{ #no offset
            fit1 <- glm(ff1,family="poisson",data=datt)
            fit2 <- glm(ff2,family="poisson",data=datt)
            eventtime <- datt[,list(sample=.N,
                                    event=sum(eval(parse(text=all.vars(object$formula)[[1]])),na.rm=TRUE)),
                              by=c(var,treatment)]
            eventtime <- data.table::dcast(eventtime,paste(var,"~",treatment),
                                           value.var=list("sample","event")) 
        }
        pinteraction <- anova(fit1,fit2,test="Chisq")$"Pr(>Chi)"[2]
    }
    else if(model=="logistic"){
        fit1 <- glm(ff1,family="binomial",data=datt)
        fit2 <- glm(ff2,family="binomial",data=datt)
        eventtime <- datt[,list(sample=.N,
                                event=sum(eval(parse(text=all.vars(object$formula)[[1]])),na.rm=TRUE)),
                          by=c(var,treatment)]
        eventtime <- data.table::dcast(eventtime,paste(var,"~",treatment),
                                       value.var=list("sample","event")) 
        pinteraction <- anova(fit1,fit2,test="Chisq")$"Pr(>Chi)"[2]
    }
    setnames(eventtime,var,"level")
    eventtime <- eventtime[!(level=="<NA>")]
    length <- dim(eventtime)[1]
    variable <- data.table(subgroups=rep(var,length))
    rt <- suppressMessages(data.table::setDT(summary(regressionTable(fit1),print=FALSE)$rawTable)[,tail(.SD,length)])
    rt <- rt[,Variable:=NULL]
    OUT <- cbind(variable,eventtime,rt,pinteraction) 
    OUT[,pinteraction:=pinteraction]
    OUT
  }
  ) ,fill=TRUE) # end do.call/rbind  
  class(Result) <- c("subgroupAnalysis","data.frame","data.table")
  Result 
}


