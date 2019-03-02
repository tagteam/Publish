#' @title plot.subgroupAnalysis
#' @description 
#' This function operates on a "subgroupAnalysis" object to produce a formatted
#' table and a forest plot
#' @author Christian Torp-Pedersen
#' @param x - a subgroupAnalysis object
#' @param ... - passed on to plotConfidence
#' @details 
#' This function produces a formatted table of a subgroupAnalysis object and
#' adds a forest plot.  If further details needs attention before plotting is
#' is advisable use adjust the table produced by the summary function and then
#' plotting with the plotConfidence function
#' @return NULL
#' @seealso subgroupAnalysis, plotConfidence
#' @export
#' @examples
#' #load libraries
#' library(Publish)
#' library(survival)
#' library(data.table)
#' data(traceR) #get dataframe traceR
#' setDT(traceR)
#' traceR[,':='(wmi2=factor(wallMotionIndex<0.9,levels=c(TRUE,FALSE), 
#'                 labels=c("bad","good")),
#'              abd2=factor(abdominalCircumference<95, levels=c(TRUE,FALSE), 
#'                 labels=c("slim","fat")))]
#' fit_cox <- coxph(Surv(observationTime,dead)~treatment,data=traceR)
#' # Selected subgroups - univariable analysis
#' sub_cox <- subgroupAnalysis(fit_cox,traceR,treatment="treatment",
#'   subgroup=c("smoking","sex","wmi2","abd2")) # subgroups as character string
#' plot(sub_cox)  
plot.subgroupAnalysis <- function(x,...)
{
  if (class(x)[1]!="subgroupAnalysis") stop("Object not of class subgroupAnalysis")
  num <- length(names(x))
  plotcols<-x[,(num-4):(num-2)]
  tabcols <-x[,1:2]
  Publish::plotConfidence(x=plotcols, labels=tabcols)
}