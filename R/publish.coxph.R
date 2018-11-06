##' Tabulize the part of the result of a Cox regression analysis which is commonly shown in publications.
##'
##' Transforms the log hazard ratios to hazard ratios and returns them
##' with confidence limits and p-values.  If explanatory variables are
##' log transformed or log2 transformed, a scaling factor is
##' multiplied to both the log-hazard ratio and its standard-error.
##' @title Tabulize hazard ratios with confidence intervals and
##'     p-values.
##' @param object A \code{coxph} object.
##' @param confint.method See \code{regressionTable}
##' @param pvalue.method See \code{regressionTable}
##' @param print If \code{FALSE} do not print results.
##' @param factor.reference See \code{regressionTable}
##' @param units See \code{regressionTable}
##' @param probindex Logical. If \code{TRUE} show coefficients on probabilistic index scale instead of hazard ratio scale.
##' @param ... passed to \code{summary.regressionTable} and also to
##'     \code{labelUnits}.
##' @return Table with hazard ratios, confidence intervals and
##'     p-values.
##' @author Thomas Alexander Gerds
##' @examples
##' library(survival)
##' data(pbc)
##' pbc$edema <- factor(pbc$edema,
##'              levels=c("0","0.5","1"), labels=c("0","0.5","1"))
##' fit = coxph(Surv(time,status!=0)~age+sex+edema+log(bili)+log(albumin),
##'             data=na.omit(pbc))
##' publish(fit)
##' ## forest plot
##' plot(publish(fit),cex=1.3)
##'
##' publish(fit,ci.digits=2,pvalue.eps=0.01,pvalue.digits=2,pvalue.stars=TRUE)
##' publish(fit,ci.digits=2,ci.handler="prettyNum",pvalue.eps=0.01,
##'         pvalue.digits=2,pvalue.stars=TRUE)
##' publish(fit, ci.digits=2, ci.handler="sprintf", pvalue.eps=0.01,
##'         pvalue.digits=2,pvalue.stars=TRUE, ci.trim=FALSE)
##' 
##' fit2 = coxph(Surv(time,status!=0)~age+sex+edema+log(bili,base=2)+log(albumin)+log(protime),
##'     data=na.omit(pbc))
##' publish(fit2)
##'
##' # with cluster variable
##' fit3 = coxph(Surv(time,status!=0)~age+cluster(sex)+edema+log(bili,base=2)
##'                                     +log(albumin)+log(protime),
##'     data=na.omit(pbc))
##' publish(fit3)
##'
##' # with strata and cluster variable
##' fit4 = coxph(Surv(time,status!=0)~age+cluster(sex)+strata(edema)+log(bili,base=2)
##'                  +log(albumin)+log(protime),
##'     data=pbc)
##' publish(fit4)
##' 
##' @export
publish.coxph <- function(object,
                          confint.method,
                          pvalue.method,
                          print=TRUE,
                          factor.reference="extraline",
                          units=NULL,
                          probindex=FALSE,
                          ...){
    if (missing(confint.method)) confint.method="default"
    if (missing(pvalue.method))
        pvalue.method=switch(confint.method,
            "robust"={"robust"},
            "simultaneous"={"simultaneous"},
            "default")
    spec <- attr(terms(object),"specials")
    cluster <- spec$cluster-1
    strata <- spec$strata-1
    # if (!is.null(cluster)) cluster <- cluster-1
    rt <- regressionTable(object,
                          noterms=c(cluster,strata),
                          confint.method=confint.method,
                          pvalue.method=pvalue.method,
                          factor.reference=factor.reference,
                          units=units,
                          probindex=probindex)
    srt <- summary.regressionTable(rt,
                                   ## digits=digits,
                                   print=FALSE,...)
    if (print==TRUE)
        publish(srt$regressionTable,...)
    invisible(srt)
}

#----------------------------------------------------------------------
### publish.coxph.R ends here
