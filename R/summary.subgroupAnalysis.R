#' @title summary.subgroupAnalysis
#' @description 
#' This function operates on a "subgroupAnalysis" object to produce a formatted
#' table.
#' @usage
#' table <- function(fit,subgroupVar="subgroupVar",SubgroupVal="SubgroupVal",
#' Treatment="Treatment", digits=3,eps=0.001,subGroupP=FALSE,keepDigital=FALSE )
#' @author Christian Torp-Pedersen
#' @param fit - a subgroupAnalysis object
#' @param subgroupVar - name for column with variables forming subgroups
#' @param SubgroupVal - name of column for levels of subgroupVar
#' @param Treatment - name of variable that defines subgroup levels
#' @param digits - number of digits for risk ratios and confidence limits
#' @param eps - lowest value of p to be shown exactly, others will be "<eps"
#' @subGroupP - by default only p value for interaction is shown. if set to 
#' TRUE then a column of p-values for each subgroup is also shown
#' @keepDigital - prevents formatting risk ratio and confidence limits. Useful
#' for cases when further manipulations of rows and columns prior to addeing a
#' forest plot is relevant.
#' @details 
#' This function produces a formatted table of a subgroupAnalysis object.
#' A forest plot can be added with the plot function.
#' @return A data.frame with formatted values for subgroups
#' @seealso subgroupAnalysis
#' @export
#' @examples
#' #load libraries
#' library(Publish)
#' library(survival)
#' data(traceR) #get dataframe traceR
#' traceR$wmi2 <- ifelse(traceR$wallMotionIndex<0.9,"bad","good")
#' traceR$abd2 <- ifelse(traceR$abdominalCircumference<95,"slim","fat")
#' # Selected subgroups - univariable analysis
#' sub <- subgroupAnalysis(traceR,outcome="dead",timevar="observationTime",
#'   treatment="treatment", subgroup=c("smoking","sex","wmi2","abd2")) 
#' sub2 <- summary(sub)  
#' sub2 
summary.subgroupAnalysis <- function(fit,subgroupVar="subgroupVar",SubgroupVal="subgroupVal",
        Treatment="Treatment", digits=3,eps=0.001,subGroupP=FALSE,keepDigital=FALSE                             
)
{
  if (class(fit)!="subgroupAnalysis") stop("Object not of class subgroupAnalysis")
  class(fit) <- "data.frame"
  fit$pinteraction <- format.pval(fit$pinteraction,eps=eps,digits=3)
  if(!keepDigital) for (i in 7:10) fit[,i] <-format(fit[,i],digits=digits)
  if (!subGroupP) fit <-fit[,-10]
  for(i in 3:4) fit[,i]<-format(fit[,i],digits=0)
  fit <- fit[,-5] #remove column named Variable
  names(fit) <- append(c(subgroupVar,SubgroupVal,"cases","events",Treatment),names(fit)[-(1:5)])
  if(keepDigital) fit[,-(6:8)] <- data.frame(lapply(fit[,-(6:8)], FUN = function(x) gsub("NA", "", x)))
  else fit <- data.frame(lapply(fit, FUN = function(x) gsub("NA", "", x)))
  fit
}