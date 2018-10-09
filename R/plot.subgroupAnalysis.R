#' @title plot.subgroupAnalysis
#' @description 
#' This function operates on a "subgroupAnalysis" object to produce a formatted
#' table and a forest plot
#' @usage
#' function(fit,subgroupVar="subgroupVar",SubgroupVal="SubgroupVal",
#' Treatment="Treatment",...)
#' @author Christian Torp-Pedersen
#' @param fit - a subgroupAnalysis object
#' @param subgroupVar - name for column with variables forming subgroups
#' @param SubgroupVal - name of column for levels of subgroupVar
#' @param Treatment - name of variable that defines subgroup levels
#' @details 
#' This function produces a formatted table of a subgroupAnalysis object and
#' adds a forest plot.  If further details needs attention before plotting is
#' is advisable use adjust the table produced by thesummary function and then
#' plotting with the plotConfidence function
#' @return A plot
#' @seealso subgroupAnalysis, plotConfidence
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
#' plot(sub)  
plot.subgroupAnalysis <- function(fit,subgroupVar="subgroupVar",SubgroupVal="subgroupVal",
        Treatment="Treatment"                            
)
{
  if (class(fit)!="subgroupAnalysis") stop("Object not of class subgroupAnalysis")
  fit <- summary.subgroupAnalysis(fit,keepDigital = TRUE)
  names(fit) <- c("Group","Level","cases","events","Treatment","RiskRatio","Lower","Upper","Pint") 
  Publish::plotConfidence(x=fit[,6:8], labels=fit[,-(6:8)])
}