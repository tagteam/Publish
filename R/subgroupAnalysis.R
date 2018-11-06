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
#' @usage
#' subgroupAnalysis <- function(dat,outcome,timevar=NULL,treatment,
#' basevars=NULL, subgroup, model="cox",confint="default",factor.reference=
#' "inline")
#' @author Christian Torp-Pedersen
#' @param dat - Dataset including all relevant variables
#' @param outcome - Variable indicating outcome, mut be numeric zero or one
#' @param timevar - Must contain the variable showing time from exposure to
#' outcome or censoring in case of a cox-model. For a poisson model of rates
#' this variable is also necessary - the program used the logarithm of time as
#' offset, so do not use the logarithm function before the curren function.
#' When not relevant, a value of NULL is appropriate.
#' @param treatment - The variable to be examined in subgroups
#' @param basevars - A set of variables, a string of variables names, to be 
#' included in all models. Should NOT include any variables from "subgroup".
#' @param subgroup - A set of variables, a string of variables names, presen-
#' ting the variables where subgroups should be formed.
#' @param model - The model to be used, "cox","logistic", "poisson"
#' @param confint - "default" creates Wald type confidence interval, "robust",
#' creates creates robust standard errors - se function regressionTable.
#' @param factor.reference - "extraline" creates an extraline for the reference,
#' "inline" avoids this line.
#' @details 
#' The function selects complete cases based on outcome, treatment and basevars-
#' and timevar when relevant. For each variable in subgroup complete cases are 
#' examined. Parameter estimates and p-values for interaction are those calcu-
#' lated by including interaction terms in "phreg" and "glm".
#' @return A data.frame with subsgroup specifications, number in each subgroup,
#' parameter estimates and p for interaction.  If a forest plot is further
#' planned this can be added with "plotConfidence"
#' @seealso coxph, glm, plotConfidence
#' @export
#' @examples
#' #load libraries
#' library(MASS)
#' library(data.table)
#' library(Publish)
#' library(survival)
#' data(traceR) # get dataframe traceR
#' traceR$wmi2 <- ifelse(traceR$wallMotionIndex<0.9,"bad","good")
#' traceR$abd2 <- ifelse(traceR$abdominalCircumference<95,"slim","fat")
#' # univariate analysis of smoking in subgroups of age and sex
#' # Basic model from randomised study - but observed for 12 years
#' summary(regressionTable(coxph(Surv(observationTime,dead)~treatment,
#'    data=traceR)))
#' # Selected subgroups - univariable analysis
#' subgroupAnalysis(traceR,outcome="dead",timevar="observationTime",
#'   treatment="treatment", subgroup=c("smoking","sex","wmi2","abd2")) 
#' # Subgroups but with adjustment for two other variables
#' subgroupAnalysis(traceR,outcome="dead",timevar="observationTime",
#'   treatment="treatment", subgroup=c("smoking","sex","wmi2","abd2"),
#'   basevars=c("age","seCreatinine"))   
#' # Analysis with Poisson - and the unrealistic assumption of constant hazard
#' subgroupAnalysis(traceR,outcome="dead",timevar="observationTime",
#'   treatment="treatment", subgroup=c("smoking","sex","wmi2","abd2"),
#'   basevars=c("age","seCreatinine"),model="poisson")       
#' # Analysis with logistic regression - and very wrongly ignoring censoring
#' subgroupAnalysis(traceR,outcome="dead",timevar=NULL,
#'   treatment="treatment", subgroup=c("smoking","sex","wmi2","abd2"),
#'   basevars=c("age","seCreatinine"),model="logistic",factor.reference="inline")   
#'    
subgroupAnalysis <- function(dat, # Data with all variables
                             outcome, # Variable with outcome
                             timevar=NULL, # For Cox analysis a variable, otherwise to be ignored
                             treatment, # Variable fro which the importance is to be compared in subgroups
                             basevars=NULL, # A set of variables always to be included in model - do not mix with subgroup!
                             subgroup, # A set of variables where interaction is determined and results in subgroups calculated
                             model="cox", # variable for type of model, currently accepted: c("cox","logistic","Poisson")
                             confint.method="default", # Wald type confidence interval
                             factor.reference="extraline"
                             )
{
  setDT(dat)
  model <- tolower(model) # just practical
  if (model=="cox" & class(timevar)!="character") stop ("Error - Cox selection requires a variable to define time")
  if ((!is.null(basevars) & class(basevars)!="character") | class(subgroup) !="character") stop("Error - variables for analyses must be defined by character names")
  if (!model %in% c("cox","logistic","poisson")) stop("Error - model selection not eligible")
  # basic formula contruction:
  covariates <-ifelse(is.null(basevars),"",paste0(paste(basevars,collapse='+'),'+'))
  if(model=="cox")
    formstart <- paste0("Surv(",timevar,",",outcome,")~",covariates)
  else formstart <- paste0(outcome,"~",covariates)
  datt <- copy(dat)
  datt <- na.omit(datt,cols=c(outcome,timevar,basevars,treatment))
  # Define empty output
  output <- read.csv(text="var2,var2val,cases,events,Variable,Units,HazardRatio,Lower,Upper,Pvalues,pinteraction")
  for(var in subgroup){ # Variable currently analyzed by values
    datt2 <- na.omit(datt,var) # complete.cases by var
    if(model=="cox"){
      fit1 <- coxph(as.formula(paste0(formstart,var,"*",treatment)),data=datt2)
      fit2 <- coxph(as.formula(paste0(formstart,var,"+",treatment)),data=datt2)   
      pinteraction <- anova(fit1,fit2)[4][2,]
    }
    else
    if(model=="poisson"){
      if(!is.null(timevar)){
        datt2[,logtime:=log(.SD),.SDcols=timevar] 
        datt2 <- datt2[!(is.infinite(logtime) | is.nan(logtime) | is.na(logtime)),]
        fit1 <- glm(as.formula(paste0(formstart,var,"+",treatment)),family="poisson",offset=logtime,data=datt2)  
        fit2 <- glm(as.formula(paste0(formstart,var,"*",treatment)),family="poisson",offset=logtime,data=datt2)  
      }
      else{
        fit1 <- glm(as.formula(paste0(formstart,var,"+",treatment)),family="poisson",offset=logtime,data=datt2)
        fit2 <- glm(as.formula(paste0(formstart,var,"*",treatment)),family="poisson",offset=logtime,data=datt2)
      }
      pinteraction <- anova(fit1,fit2,test="Chisq")$"Pr(>Chi)"[2]
    } 
    else
    if(model=="logistic"){
      fit1 <- glm(as.formula(paste0(formstart,var,"+",treatment)),family="binomial",data=datt2)  
      fit2 <- glm(as.formula(paste0(formstart,var,"*",treatment)),family="binomial",data=datt2) 
      pinteraction <- anova(fit1,fit2,test="Chisq")$"Pr(>Chi)"[2]
    } 
    values <- unique(datt2[,.SD,.SDcols=var])[[1]] #values of current subgroup variable
    for (j in 1:length(values)){ #Iterate through values of each subgrouping variable
      datt3 <- subset(datt2,eval(parse(text = var))==values[j]) #Select relevant part of data
      cases <- dim(datt3)[1] #number of cases
      events <- datt3[,.SD,.SDcols=outcome][,sum(.SD),.SDcols=outcome]#number of events
      if(model=="cox")
        fit2 <- coxph(as.formula(paste0(formstart,treatment)),data=datt3)
      else
      if(model=="poisson"){
        if(!is.null(timevar)) 
            fit2 <- glm(as.formula(paste0(formstart,treatment)),family="poisson",offset=logtime,data=datt2) 
        else
            fit2 <- glm(as.formula(paste0(formstart,treatment)),family="poisson",data=datt2) 
      }
      else
      if(model=="logistic")
        fit2 <- glm(as.formula(paste0(formstart,treatment)),family="binomial",data=datt2) 
      # Create regression table for subgroup
      fit2 <-summary(regressionTable(fit2),print=FALSE, confint.method=confint.method,
          factor.reference=factor.reference)$rawTable #Model for subgroup     
      if(j==1){ #Only put Name and p for interaction on first line
        var2<-var #Name of current subset variable and p-interact for first row in group
        p <- pinteraction
      }
      else{
        var2<-""
        p<-NA   
      } 
      leftpart <- read.csv(text="var2,valvar2,cases,events") #Data to the left of regression table
      rightpart <- read.csv(text="pinteraction") # Data to the right of regression table
      for(k in 1:dim(fit2)[1]){ # For each row of regressionTable
        if(k==1){
          var2val <- values[j]
          add.left <- data.frame(var2=var2,var2val=var2val,cases=cases,events=events)
          add.right <- data.frame(pinteraction=p)
        }
        else{
          add.left <- data.frame(var2="",var2val="",cases=NA,events=NA)
          add.right <- data.frame(pinteraction=NA)
        }
        leftpart <- rbind(leftpart,add.left)  
        rightpart <- rbind(rightpart,add.right)  
      }
      fit2 <- cbind(leftpart,fit2,rightpart)  
      output <- rbind(output,fit2)
    } # end for loop with j
   } # end for var
  output <- data.frame(output)
  class(output) <- "subgroupAnalysis"
  output
}
  



