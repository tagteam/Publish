#' @title Subgroup Analysis - Interactions and estimates
#' @description 
#' In many randomised and observational studies a set of analyses by subgroups
#' is performed. In the subgroups one variable is of particular interest and it
#' is requested to tabulate parameter estimates of that variable in subgroups as
#' well as the probability of interaction.  This function can generate a table
#' where rows represent sequential subgroups by a range of variables as well as
#' parameter estimates for a variable of particular interest - and the probabi-
#' lity of interaction.  The function can examine Cox-models, logistic models 
#' and Poisson models. 
#' @author Christian Torp-Pedersen
#' @usage
#' subgroupAnalysis(object,dat,timevar=NULL,treatment,
#'    subgroup, confint.method="default",factor.reference="extraline")
#' @param object - glm, coxph or cph object for which subgroups should be
#' analyzed.
#' @param dat - Dataset including all relevant variables
#' @param timevar - Must contain the variable showing observation time for for 
#' a poisson model of rates - the program used the logarithm of time as
#' offset, so do not use the logarithm function before the current function.
#' When not relevant, leave the default value of NULL.
#' @param treatment - The variable to be examined in subgroups, best coded
#'   as 0/1 numeric.
#' @param subgroup - A set of variables, a string of variables names, presen-
#' ting the variables where subgroups should be formed. These variables should
#' all be "factor"
#' @param confint.method - "default" creates Wald type confidence interval, "robust",
#' creates creates robust standard errors - see regressionTable function.
#' @param factor.reference - "extraline" creates an extraline for the reference,
#' "inline" avoids this line.
#' @details 
#' The function can only hand a bivariate outcome, most conviniently coded as
#' zero or one.  The probability of interaction is from a likelihood ratio test
#' of nested models with and without the subgroup identifier of interest. Esti-
#' mates of subgroup effects are from the regressionTable funtion.
#' @return A data.frame with subsgroup specifications, number in each subgroup,
#' parameter estimates and p for interaction.  If a forest plot is further
#' planned this can be added with "plotConfidence"
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
#' # univariate analysis of smoking in subgroups of age and sex
#' # Basic model from randomised study - but observed for 12 years
#' fit_cox <- coxph(Surv(observationTime,dead)~treatment,data=traceR)
#' # Selected subgroups - univariable analysis
#' sub_cox <- subgroupAnalysis(fit_cox,traceR,treatment="treatment",
#'   subgroup=c("smoking","sex","wmi2","abd2")) # subgroups as character string
#' # When both start and end are in the Surv statement:
#' traceR[,null:=0]
#' fit_cox2 <- coxph(Surv(null,observationTime,dead)~treatment,data=traceR)
#' summary(regressionTable(fit_cox))
#' sub_cox2 <- subgroupAnalysis(fit_cox2,traceR,treatment="treatment",
#'   subgroup=c("smoking","sex","wmi2","abd2")) # subgroups as character string
#' # Analysis with Poisson - and the unrealistic assumption of constant hazard
#' # and adjusted for age in all subgroups
#' fit_p <- glm(dead~treatment+age,family="poisson", 
#'   offset=log(observationTime),data=traceR)
#' sub_pois <- subgroupAnalysis(fit_p,traceR,treatment="treatment",
#'   timevar="observationTime",
#'   subgroup=~smoking+sex+wmi2+abd2) 
#' # Analysis with logistic regression - and very wrongly ignoring censoring
#' fit_log <- glm(dead~treatment+age,family="binomial",data=traceR)
#' sub_log <- subgroupAnalysis(fit_log,traceR,treatment="treatment",timevar=NULL,
#'    subgroup=~smoking+sex+wmi2+abd2, factor.reference="inline")
subgroupAnalysis <- function(object, # glm, lrm, coxph or cph object
                             dat, # data with all variables
                             timevar=NULL, # timevariable necessaary for Poisson models
                             treatment, # max 2 values
                             subgroup, # Character vector or Formula. Factor list of subgroups variables
                             confint.method="default", # Wald type confidence interval
                             factor.reference="extraline"){
  timevar__=level=tail=Variable=NULL
  if(!(class(object)[1] %in% c("coxph","cph","glm"))) stop ("Error - Object must be coxph, cph or glm")
  if(!(class(treatment)=="character")) stop("Error - Variable treament must be character")
  if(class(subgroup)=="formula") subgroup <- all.vars(subgroup)
  else if(!(class(subgroup)=="character")) stop ("Error - subgroup must be formula or character")
  if (!class(dat)[1] %in% c("data.frame","data.table")) stop ("Error - dat must be data.frame og data.table")
  else{
    datt <- copy(dat)
    data.table::setDT(datt)
  }
  if (!all(stats::complete.cases(dat[,.SD,.SDcols=c(subgroup,all.vars(object$formula),treatment)]))) 
    warning("dat has missing values in columns used, may cause problems")
  if (!treatment %in% all.vars(object$formula)) stop("Error - treatment must be in the formula")
  #Define type of analysis
  if (class(object)[1] %in% c("coxph","cph")) model<-"cox"
  else if (class(object)[1]=="glm"){
    if(object$family$family=="binomial") model<-"logistic"
    else if (object$family$family=="poisson") model<-"poisson" # Poisson no offset
    else stop("Error - type of study not an option or misspecified")
  }
  #subgroup variables should not be in the models
  for (i in all.vars(object$formula)) if (i %in% subgroup)
    stop("Subgroup variables should not be part of every model")
  Result <- rbindlist(lapply(subgroup,function(var){
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
        offsetvar <- data.table(timevar__=object$offset)
        datt <- cbind(datt,offsetvar)
        fit1 <- glm(ff1,family="poisson",offset=timevar__,data=datt)
        fit2 <- glm(ff2,family="poisson",offset=timevar__,data=datt)
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
    variable <- data.table(subgroup=rep(var,length))
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


