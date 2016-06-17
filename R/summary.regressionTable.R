##' Preparing regression results for publication
##'
##' @title Formatting regression tables
##' @param object object obtained with \code{regressionTable}.
##' @param showMissing Decide if number of missing values are shown.
##' Either logical or character. If \code{'ifany'} then number missing values are
##' shown if there are some.
##' @param print If \code{TRUE} print results.
##' @param ... Used to control formatting of parameter estimates,
##' confidence intervals and p-values. See examples.
##' @return Formatted regression table with raw values as attributes
##' @seealso publish.glm publish.coxph 
##' @examples
##' library(survival)
##' data(pbc)
##' pbc$edema <- factor(pbc$edema,levels=c("0","0.5","1"),labels=c("0","0.5","1"))
##' fit = coxph(Surv(time,status!=0)~age+sex+edema+log(bili)+log(albumin)+log(protime),
##'             data=pbc)
##' summary(regressionTable(fit))
##' summary(regressionTable(fit),handler="prettyNum")
##' summary(regressionTable(fit),handler="format")
##' summary(regressionTable(fit),handler="sprintf",digits=c(2,2),pValue.stars=TRUE)
#' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
summary.regressionTable <- function(object,
                                    showMissing="ifany",
                                    print=TRUE,
                                    ...){
    pynt <- getPyntDefaults(list(...),names=list("digits"=c(2,3),"handler"="sprintf",nsmall=NULL))
    digits <- pynt$digits
    handler <- pynt$handler
    if (length(digits)==1) digits <- rep(digits,2)
    if (length(pynt$nsmall)>0) nsmall <- pynt$nsmall else nsmall <- pynt$digits
    Rtab <- do.call("rbind",object)
    Lower <- Rtab[,"Lower"]
    Upper <- Rtab[,"Upper"]
    Pvalue <- Rtab[,"Pvalue"]
    Rtab <- Rtab[,-match(c("Lower","Upper","Pvalue"),colnames(Rtab)),drop=FALSE]
    pvalue.defaults <- list(digits=digits[[2]],
                            eps=10^{-digits[[2]]},
                            stars=FALSE)
    ci.defaults <- list(format="[l;u]",
                        digits=digits[[1]],
                        nsmall=digits[[1]],
                        degenerated="asis")
    smartF <- prodlim::SmartControl(call=list(...),
                                    keys=c("ci","pvalue"),
                                    ignore=c("object","print","handler","digits","nsmall"),
                                    defaults=list("ci"=ci.defaults,"pvalue"=pvalue.defaults),
                                    forced=list("ci"=list(lower=Lower,
                                                    upper=Upper,
                                                    handler=handler,
                                                    digits=digits[[1]],
                                                    nsmall=nsmall[[1]]),
                                        "pvalue"=list(Pvalue)),
                                    verbose=FALSE)
    if (attr(object,"model")%in%c("Cox regression","Poisson regression")){
        attr(Rtab,"model") <- "Cox regression"
        if (match("ProbIndex",colnames(Rtab),nomatch=0)){
            attr(Rtab,"ProbIndex") <- Rtab[,"ProbIndex"]
            Rtab$ProbIndex <- pubformat(Rtab$ProbIndex,handler=handler,digits=digits[[1]],nsmall=nsmall[[1]])
        } else{
              attr(Rtab,"HazardRatio") <- Rtab[,"HazardRatio"]
              Rtab$HazardRatio <- pubformat(Rtab$HazardRatio,handler=handler,digits=digits[[1]],nsmall=nsmall[[1]])
          }
    }else{
         if (attr(object,"model")=="Logistic regression"){
             attr(Rtab,"model") <- "Logistic regression"
             attr(Rtab,"OddsRatio") <- Rtab[,"OddsRatio"]
             Rtab$OddsRatio <- pubformat(Rtab$OddsRatio,handler=handler,digits=digits[[1]],nsmall=nsmall[[1]])
         } else{
               ## assume "Linear regression"
               attr(Rtab,"model") <- "Linear regression"
               attr(Rtab,"Coefficient") <- Rtab[,"Coefficient"]
               Rtab$Coefficient <- pubformat(Rtab$Coefficient,handler=handler,digits=digits[[1]],nsmall=nsmall[[1]])
           }
     }
    Rtab$CI.95 <- do.call("formatCI",smartF$ci)
    Rtab$"p-value" <- do.call("format.pval",smartF$pvalue)
    if (length(smartF$pvalue$stars)>0 && smartF$pvalue$stars==TRUE)
        Rtab$signif <- symnum(Pvalue,corr = FALSE,na = FALSE,cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),symbols = c("***", "**", "*", ".", " "))
    attr(Rtab,"Lower") <- Lower
    attr(Rtab,"Upper") <- Upper
    attr(Rtab,"Pvalue") <- Pvalue
    rownames(Rtab) <- NULL
    ##
    if (showMissing=="ifany") showMissing <- !all(Rtab[,"Missing"] %in% c("","0"))
    if (!showMissing)
        Rtab <- Rtab[,-match("Missing",colnames(Rtab))]
    if (print==TRUE) {
        print(Rtab,...)
        ## if (smartF$pvalue$stars==TRUE)
        ## cat("\nSignif. codes:  0 '***'0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
    }
    invisible(Rtab)
}
