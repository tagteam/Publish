##' Preparing regression results for publication
##'
##' @title formatting regression tables
##' @param x
##' @param digits
##' @param nsmall
##' @param print
##' @param ...
##' @return Formatted regression table with raw values as attributes
##' @seealso publish.glm publish.coxph 
##' @examples
##' library(survival)
##' data(pbc)
##' pbc$edema <- factor(pbc$edema,levels=c("0","0.5","1"),labels=c("0","0.5","1"))
##' fit = coxph(Surv(time,status!=0)~age+sex+edema+log(bili)+log(albumin)+log(protime),
##'             data=pbc)
##' summary(regressionTable(fit))
#' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
summary.regressionTable <- function(x,digits=2,nsmall=2,print=TRUE,...){
    Rtab <- do.call("rbind",x)
    Lower <- Rtab[,"Lower"]
    Upper <- Rtab[,"Upper"]
    Pvalue <- Rtab[,"Pvalue"]
    Rtab <- Rtab[,-match(c("Lower","Upper","Pvalue"),colnames(Rtab)),drop=FALSE]
    pval.defaults <- list(digits=4,eps=0.0001)
    ci.defaults <- list(format="[l;u]",digits=digits,nsmall=digits,degenerated="asis")
    smartF <- prodlim::SmartControl(call=list(...),
                                    keys=c("ci","pval"),
                                    ignore=c("x","print"),
                                    defaults=list("ci"=ci.defaults,"pval"=pval.defaults),
                                    forced=list("ci"=list(lower=Lower,upper=Upper),"pval"=list(Pvalue)),
                                    verbose=TRUE)
    if (attr(x,"model")=="Cox regression"){
        attr(Rtab,"model") <- "Cox regression"
        attr(Rtab,"HazardRatio") <- Rtab[,"HazardRatio"]
        Rtab$HazardRatio <- format(Rtab$HazardRatio,digits=digits,nsmall=nsmall)
    } else{
        if (attr(x,"model")=="Logistic regression"){
            attr(Rtab,"model") <- "Logistic regression"
            attr(Rtab,"OddsRatio") <- Rtab[,"OddsRatio"]
            Rtab$OddsRatio <- format(Rtab$OddsRatio,digits=digits,nsmall=nsmall)
        } else{
            ## assume "Linear regression"
            attr(Rtab,"model") <- "Linear regression"
            attr(Rtab,"Coefficient") <- Rtab[,"Coefficient"]
            Rtab$Coefficient <- format(Rtab$Coefficient,digits=digits,nsmall=nsmall)
        }}
    Rtab$CI.95 <- do.call("formatCI",smartF$ci)
    Rtab$"p-value" <- do.call("format.pval",smartF$pval)
    attr(Rtab,"Lower") <- Lower
    attr(Rtab,"Upper") <- Upper
    attr(Rtab,"Pvalue") <- Pvalue
    rownames(Rtab) <- NULL
    if (print==TRUE) print(Rtab,...)
    invisible(Rtab)
}
