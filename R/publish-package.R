#' A study was made of all 26 astronauts on the first eight space shuttle flights (Bungo et.al., 1985).
#' On a voluntary basis 17 astronauts consumed large quantities of salt and fluid prior to landing as
#' a countermeasure to space deconditioning, while nine did not.

#' @name SpaceT
#' @docType data
#' @format A data frame with 52 observations on the following 4 variables:
#' \describe{
#' \item{Status}{Factor with levels Post (after flight) and Pre (before flight)}
#' \item{HR}{Supine heart rate(beats per minute)}
#' \item{Treatment}{Countermeasure salt/fluid (1= yes, 0=no)}
#' \item{ID}{Person id}
#' }
#' @references
#' Altman, Practical statistics for medical research, Page 223, Ex. 9.1.
#' Bungo et.al., 1985
#' @examples
##' data(SpaceT)
NULL
#' Diabetes data of Dr John Schorling
#'
#' These data are courtesy of Dr John Schorling, Department of Medicine, University of Virginia School of Medicine.
#' The data consist of 19 variables on 403 subjects from 1046 subjects who were interviewed in a study to understand
#' the prevalence of obesity, diabetes, and other cardiovascular risk factors in central Virginia for African Americans.
#' According to Dr John Hong, Diabetes Mellitus Type II (adult onset diabetes) is associated most strongly with obesity.
#' The waist/hip ratio may be a predictor in diabetes and heart disease. DM II is also agssociated with hypertension -
#' they may both be part of "Syndrome X". The 403 subjects were the ones who were actually screened for diabetes.
#' Glycosolated hemoglobin > 7.0 is usually taken as a positive diagnosis of diabetes.
#'
#' @name Diabetes
#' @docType data
#' @format A data frame with 205 observations on the following 12 variables.
#' \describe{
#' \item{id}{subject id}
#' \item{chol}{Total Cholesterol}
#' \item{stab.glu}{Stabilized Glucose}
#' \item{hdl}{High Density Lipoprotein}
#' \item{ratio}{Cholesterol/HDL Ratio}
#' \item{glyhb}{Glycosolated Hemoglobin}
#' \item{location}{a factor with levels (Buckingham,Louisa)}
#' \item{age}{age (years)}
#' \item{gender}{male or female}
#' \item{height}{height (inches)}
#' \item{height.europe}{height (cm)}
#' \item{weight}{weight (pounds)}
#' \item{weight.europe}{weight (kg)}
#' \item{frame}{a factor with levels (small,medium,large)}
#' \item{bp.1s}{First Systolic Blood Pressure}
#' \item{bp.1d}{First Diastolic Blood Pressure}
#' \item{bp.2s}{Second Diastolic Blood Pressure}
#' \item{bp.2d}{Second Diastolic Blood Pressure}
#' \item{waist}{waist in inches}
#' \item{hip}{hip in inches}
#' \item{time.ppn}{Postprandial Time when Labs were Drawn in minutes}
#' \item{AgeGroups}{Categorized age}
#' \item{BMI}{Categorized BMI}
#' }
#' @references
#' Willems JP, Saunders JT, DE Hunt, JB Schorling: Prevalence of coronary heart disease risk factors among rural blacks: A community-based study. Southern Medical Journal 90:814-820; 1997
#' Schorling JB, Roach J, Siegel M, Baturka N, Hunt DE, Guterbock TM, Stewart HL: A trial of church-based smoking cessation interventions for rural African Americans. Preventive Medicine 26:92-101; 1997.
#' @source
#' \url{http://192.38.117.59/~tag/Teaching/share/data/Diabetes.html}
#' @keywords datasets
##' @examples
##'
##' data(Diabetes)
##'
NULL

#' trace data
#'
#' These data are from screening to the TRACE study, a comparison between the angiotensin converting
#' enzyme inhibitor trandolapril and placebo ford large myocardial infarctions. A total of 6676
#' patients were screened for the study. Survival has been followed for the screened population for
#' 16 years. The current data has been prepared for a poisson regression to examine survival. The data
#' has been "split" in 0.5 year intervals (plitLexis function from Epi package) and then collapsed
#' on all variables (aggregate function).
#' @name trace
#' @docType data
#' @format A data frame with 1832 observations on the following 6 variables.
#' \describe{
#' \item{Time}{Time after myocardial infarction, in 6 months intervals}
#' \item{smoking}{Smoking status. A factor with levels (Never, Current, Previous)}
#' \item{sex}{A factor with levels (Female, Male)}
#' \item{age}{Age in years at the time of myocardial infarction}
#' \item{ObsTime}{Cumulative risk time in each split}
#' \item{dead}{Count of deaths}
#' }
#' @references
#' Kober et al 1995 Am. J. Cardiol 76,1-5
#'
#' @keywords datasets
##' @examples
##'
##' data(trace)
##' Units(trace,list("age"="years"))
##' fit <- glm(dead ~ smoking+sex+age+Time+offset(log(ObsTime)), family="poisson",data=trace)
##' rtf <- regressionTable(fit,factor.reference = "inline")
##' summary(rtf)
##' publish(fit)
##'
NULL

#' CiTable data
#'
#' These data are used for testing Publish package functionality.
#' @name CiTable
#' @docType data
#' @format A data frame with 27 observations on the following 9 variables.
#' \describe{
#' \item{Drug}{}
#' \item{Time}{}
#' \item{Drug.Time}{}
#' \item{Dose}{}
#' \item{Mean}{}
#' \item{SD}{}
#' \item{n}{}
#' \item{HazardRatio}{}
#' \item{lower}{}
#' \item{upper}{}
#' \item{p}{}
#' }
#'
#' @keywords datasets
##' @examples
##'
##' data(CiTable)
##' labellist <- split(CiTable[,c("Dose","Mean","SD","n")],CiTable[,"Drug"])
##' labellist
##' plotConfidence(x=CiTable[,c("HazardRatio","lower","upper")], labels=labellist)
##' 
##'
NULL

#' Publish package
#'
#' This package processes results of descriptive statistcs and regression analysis into final tables and figures of a manuscript
#' @docType package
#' @name Publish-package
#' @importFrom data.table as.data.table copy data.table is.data.table melt rbindlist setnames setorder setcolorder setkey ":=" ".N" ".SD"
NULL

#' traceR data
#'
#' These data are from the TRACE randomised trial,  a comparison between the angiotensin converting
#' enzyme inhibitor trandolapril and placebo ford large myocardial infarctions. In all, 1749 patients
#' were randomised. The current data are from a 15 year follow-up. 
#' @name traceR
#' @docType data
#' @format A data frame with 1749 observations on the following variables.
#' \describe{
#' \item{weight}{Weight in kilo}
#' \item{height}{Height in meters}
#' \item{abdominalCircumference}{in centimeters}
#' \item{seCreatinine}{in mmol per liter}
#' \item{wallMotionIndex}{left ventricular function 0-2, 0 worst, 2 normal}
#' \item{observationTime}{time to death or censor}
#' \item{age}{age in years}
#' \item{sex}{0=female,1=male}
#' \item{smoking}{0=never,1=prior,2=current}
#' \item{dead}{0=censor,1=dead}
#' \item{treatment}{placebo or trandolapril}
#' 
#' }
#' @references
#' Kober et al 1995 NEJM 333,1670
#'
#' @keywords datasets
##' @examples
##'
##' data(trace)
##' Units(trace,list("age"="years"))
##' fit <- glm(dead ~ smoking+sex+age+Time+offset(log(ObsTime)), family="poisson",data=trace)
##' rtf <- regressionTable(fit,factor.reference = "inline")
##' summary(rtf)
##' publish(fit)
##'
NULL

