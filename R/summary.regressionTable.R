##' Preparing regression results for publication
##'
##' @title formatting regression tables
##' @param x
##' @param digits Vector of length 2 (second for pvalues, first for all other). 
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
summary.regressionTable <- function(x,digits=2,nsmall=2,handler="sprintf",print=TRUE,...){
    Rtab <- do.call("rbind",x)
    Lower <- Rtab[,"Lower"]
    Upper <- Rtab[,"Upper"]
    Pvalue <- Rtab[,"Pvalue"]
    Rtab <- Rtab[,-match(c("Lower","Upper","Pvalue"),colnames(Rtab)),drop=FALSE]
    pvalue.defaults <- list(digits=4,eps=0.0001)
    ci.defaults <- list(format="[l;u]",
                        digits=digits,
                        nsmall=digits,
                        degenerated="asis")
    smartF <- prodlim::SmartControl(call=list(...),
                                    keys=c("ci","pvalue"),
                                    ignore=c("x","print"),
                                    defaults=list("ci"=ci.defaults,"pvalue"=pvalue.defaults),
                                    forced=list("ci"=list(lower=Lower,upper=Upper),"pvalue"=list(Pvalue)),
                                    verbose=TRUE)
    if (handler=="sprintf"){ fmt <- paste0("%1.",digits[[1]],"f")}
    if (attr(x,"model")=="Cox regression"){
        attr(Rtab,"model") <- "Cox regression"
        attr(Rtab,"HazardRatio") <- Rtab[,"HazardRatio"]
        if (handler=="sprintf"){
            Rtab$HazardRatio <- sprintf(fmt=fmt,Rtab$HazardRatio)
        }else{
            Rtab$HazardRatio <- do.call(handler,list(Rtab$HazardRatio,digits=digits,nsmall=nsmall))
        }
    } else{
        if (attr(x,"model")=="Logistic regression"){
            attr(Rtab,"model") <- "Logistic regression"
            attr(Rtab,"OddsRatio") <- Rtab[,"OddsRatio"]
            if (handler=="sprintf"){
                Rtab$OddsRatio <- sprintf(fmt=fmt,Rtab$OddsRatio)
            }else{
                Rtab$OddsRatio <- do.call(handler,list(Rtab$OddsRatio,digits=digits,nsmall=nsmall))
            }
        } else{
            ## assume "Linear regression"
            attr(Rtab,"model") <- "Linear regression"
            attr(Rtab,"Coefficient") <- Rtab[,"Coefficient"]
            if (handler=="sprintf"){
                Rtab$Coefficient <- sprintf(fmt=fmt,Rtab$Coefficient)
            }else{
                Rtab$Coefficient <- do.call(handler,list(Rtab$Coefficient,digits=digits,nsmall=nsmall))
            }
        }
    }
    Rtab$CI.95 <- do.call("formatCI",smartF$ci)
    Rtab$"p-value" <- do.call("format.pval",smartF$pvalue)
    if (smartF$pvalue$stars==TRUE)
        Rtab$signif <- symnum(Pvalue,corr = FALSE,na = FALSE,cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),symbols = c("***", "**", "*", ".", " "))
    attr(Rtab,"Lower") <- Lower
    attr(Rtab,"Upper") <- Upper
    attr(Rtab,"Pvalue") <- Pvalue
    rownames(Rtab) <- NULL
    if (print==TRUE) {
        print(Rtab,...)
        ## if (smartF$pvalue$stars==TRUE)
        ## cat("\nSignif. codes:  0 '***'0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
    }
    invisible(Rtab)
}
