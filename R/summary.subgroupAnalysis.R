#' @title summary.subgroupAnalysis
#' @description 
#' This function operates on a "subgroupAnalysis" object to produce a formatted
#' table.
#' @author Christian Torp-Pedersen
#' @param object - a subgroupAnalysis object
#' @param digits - number of digits for risk ratios 
#' @param eps - lowest value of p to be shown exactly, others will be "<eps"
#' @param subgroup.p - present p-values for analyses in subgroups
#' @param keep.digital - prevents formatting risk ratio and confidence limits. Useful
#' for cases when further manipulations of rows and columns prior to adding a
#' forest plot is relevant.
#' @param ... - not currently used
#' @details 
#' This function produces a formatted or unformatted table of a subgroupAnalysis object.
#' A forest plot can be added with the plot function.
#' @return A data.frame with formatted values for subgroups
#' @seealso subgroupAnalysis
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
#' traceR[,sex:=as.factor(sex)] # all subgroup variables needs to be factor                
#' traceR[observationTime==0,observationTime:=1]           
#' # univariate analysis of smoking in subgroups of age and sex
#' # Basic model from randomised study - but observed for 12 years
#' fit_cox <- coxph(Surv(observationTime,dead)~treatment,data=traceR)
#' sub_cox <- subgroupAnalysis(fit_cox,traceR,treatment="treatment",
#'   subgroup=c("smoking","sex","wmi2","abd2")) # subgroups as character string
#' summary(sub_cox)   
summary.subgroupAnalysis <- function(object,digits=3,eps=0.001,subgroup.p=FALSE,keep.digital=FALSE,...)                             
{
  dup=subgroup=dup=Pvalue=Lower=Upper=pinteraction=NULL
  if (class(object)[1]!="subgroupAnalysis") stop("Object not of class subgroupAnalysis")
  fitt <- copy(object)
  data.table::setDT(fitt)
  fitt[,dup:=duplicated(subgroup)]
  fitt[dup==TRUE,':='(subgroup='')]
  if (max(grepl("Units",names(fitt)))==1)
    fitt[,c(names(fitt)[grepl("Units",names(fitt))]):=NULL] #remove Units and Units missing
  if (max(grepl("Missing",names(fitt)))==1)
    fitt[,c(names(fitt)[grepl("Missing",names(fitt))]):=NULL] #remove Units and Units missing
  riskname <- names(fitt)[length(names(fitt))-5]# hazardRatio or ODDSratio
  if (subgroup.p & !keep.digital) fitt[,Pvalue:=format.pval(Pvalue,eps=eps,digits=3)]
  else  fitt[,Pvalue:=NULL]
  if (!keep.digital){
    fitt[,':='(riskname=format(eval(parse(text=riskname)),digits=digits),
              confint= paste0(format(Lower,digits=digits),'-',format(Upper,digits=digits)),           
              pinteraction=format.pval(pinteraction,eps=eps,digits=3))]
    setnames(fitt,"riskname",riskname)
    fitt[dup==TRUE,pinteraction:='']
    fitt[,c("Lower","Upper"):=NULL]
  } 
  fitt[,dup:=NULL]
  fitt[]                
}



