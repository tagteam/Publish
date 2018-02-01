##' Preparing regression results for publication
##'
##' @title Formatting regression tables
##' @aliases summary.regressionTable print.summary.regressionTable
##' @param object object obtained with \code{regressionTable} or \code{summary.regressionTable}.
##' @param show.missing Decide if number of missing values are shown.
##' Either logical or character. If \code{'ifany'} then number missing values are
##' shown if there are some.
##' @param print If \code{TRUE} print results.
##' @param ... Used to control formatting of parameter estimates,
##' confidence intervals and p-values. See examples.
##' @return List with two elements:
##' \itemize{
##' \item regressionTable: the formatted regression table (a data.frame)
##' \item rawTable: table with the unformatted values (a data.frame)
##' }
##' @seealso publish.glm publish.coxph 
##' @examples
##' library(survival)
##' data(pbc)
##' pbc$edema <- factor(pbc$edema,levels=c("0","0.5","1"),labels=c("0","0.5","1"))
##' fit = coxph(Surv(time,status!=0)~age+sex+edema+log(bili)+log(albumin)+log(protime),
##'             data=pbc)
##' u=summary(regressionTable(fit))
##' u$regressionTable
##' u$rawTable
##' summary(regressionTable(fit),handler="prettyNum")
##' summary(regressionTable(fit),handler="format")
##' summary(regressionTable(fit),handler="sprintf",digits=c(2,2),pValue.stars=TRUE)
#' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
summary.regressionTable <- function(object,
                                    show.missing="ifany",
                                    print=TRUE,
                                    ...){
    pynt <- getPyntDefaults(list(...),names=list("digits"=c(2,3),"handler"="sprintf",nsmall=NULL))
    digits <- pynt$digits
    handler <- pynt$handler
    if (length(digits)==1) digits <- rep(digits,2)
    if (length(pynt$nsmall)>0) nsmall <- pynt$nsmall else nsmall <- pynt$digits
    rawtab <- do.call("rbind",object)
    Rtab <- rawtab[,-match(c("Lower","Upper","Pvalue"),colnames(rawtab)),drop=FALSE]
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
                                    forced=list("ci"=list(lower=rawtab[,"Lower"],
                                                          upper=rawtab[,"Upper"],
                                                          handler=handler,
                                                          digits=digits[[1]],
                                                          nsmall=nsmall[[1]]),
                                                "pvalue"=list(rawtab[,"Pvalue"])),
                                    verbose=FALSE)
    if (attr(object,"model")%in%c("Cox regression","Poisson regression")){
        model <- "Cox regression"
        if (match("ProbIndex",colnames(Rtab),nomatch=0)){
            Rtab$ProbIndex <- pubformat(Rtab$ProbIndex,handler=handler,digits=digits[[1]],nsmall=nsmall[[1]])
        } else{
            Rtab$HazardRatio <- pubformat(Rtab$HazardRatio,handler=handler,digits=digits[[1]],nsmall=nsmall[[1]])
        }
    }else{
        if (attr(object,"model")=="Logistic regression"){
            model <- "Logistic regression"
            Rtab$OddsRatio <- pubformat(Rtab$OddsRatio,handler=handler,digits=digits[[1]],nsmall=nsmall[[1]])
        } else{
            ## assume "Linear regression"
            model <- "Linear regression"
            Rtab$Coefficient <- pubformat(Rtab$Coefficient,handler=handler,digits=digits[[1]],nsmall=nsmall[[1]])
        }
    }
    Rtab$CI.95 <- do.call("formatCI",smartF$ci)
    pp <- do.call("format.pval",smartF$pvalue)
    if (length(gpp <- grepl("<",pp))) pp[!gpp] <- paste0("  ",pp[!gpp])
    Rtab$"p-value" <- pp
    if (length(smartF$pvalue$stars)>0 && smartF$pvalue$stars==TRUE)
        Rtab$signif <- symnum(rawtab[,"Pvalue"],corr = FALSE,na = FALSE,cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),symbols = c("***", "**", "*", ".", " "))
    rownames(Rtab) <- NULL
    rownames(rawtab) <- NULL
    ## e.g., MIresults do not have a column Missing but use Imputed 
    if (match("Missing",colnames(Rtab),nomatch=0)>0){
        if (show.missing=="ifany") {
            show.missing <- any(!(Rtab[,"Missing"][!is.na(Rtab[,"Missing"])] %in% c("","0")))
        }
        if (!show.missing){
            Rtab <- Rtab[,-match("Missing",colnames(Rtab))]
            rawtab <- rawtab[,-match("Missing",colnames(rawtab))]
        }
    }
    ## reference lines
    nv <- length(Rtab$Variable)
    if (nv>1){
        if (attr(object,"factor.reference")=="extraline"){
            ppos <- match("p-value",names(Rtab))
            for (r in 1:(nv-1)){
                if (Rtab$Variable[r]!="" && Rtab$Variable[r+1]=="")
                    Rtab[r,((ppos-2):ppos)] <- c("Ref","","")
            }
        }
    }
    ## cat("\nSignif. codes:  0 '***'0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1\n")
    res <- list(regressionTable=Rtab,
                rawTable=rawtab,
                model=model,
                blocks=sapply(object,NROW))
    class(res) <- c("summary.regressionTable")
    if (print) print(res)
    res
}
#' @export 
print.summary.regressionTable <- function(x,...){
    print(x$regressionTable)
    invisible(x$regressionTable)
}
    
