##' Publish provides summary functions for data
##' and results of statistical analysis in ready-for-publication
##' design
##'
##' Some warnings are currently suppressed.
##' @title Publishing tables and figures
##' @param object object to be published 
##' @param ... Passed to method.
#' @importFrom survival Surv coxph
#' @importFrom prodlim Hist getEvent
#' @importFrom data.table set
#' @importFrom grDevices dev.size 
#' @importFrom graphics abline par plot polygon rect segments strwidth   
#' @importFrom stats anova binom.test binomial chisq.test coef confint delete.response fisher.test get_all_vars glm kruskal.test model.frame model.response na.omit na.pass naprint pchisq pt qnorm qt quantile symnum terms update update.formula var
##' @seealso publish.CauseSpecificCox publish.ci publish.coxph publish.glm publish.riskRegression publish.survdiff
##' @return Tables and figures
##' @author Thomas A. Gerds  <tag@@biostat.ku.dk>
##' @export
publish <- function (object, ...) {
  UseMethod("publish")
}
