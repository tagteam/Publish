publish.cox.aalen <- function(object,
                              conf.int = 0.95,
                              robust = FALSE,
                              scale=1,
                              handler="sprintf",
                              digits=c(2,3),
                              nsmall=digits,
                              print=TRUE,
                              ...){
    coef.matrix <- coef(object)
    # ----------------------time-constant variables----------------------
    beta <- coef.matrix[ ,"Coef."]
    beta <- beta * scale
    if (robust) se <- coef.matrix[ ,"Robust SE"]
    else se <- coef.matrix[ ,"SE"]
    se <- se * scale
    # conf int constant
    z <- qnorm((1 + conf.int)/2, 0, 1)
    # nice variable names
    const.name <- rownames(coef.matrix)
    get.names <- substr(const.name,6,nchar(const.name))
    get.names <- strsplit(get.names,")")
    covNames <- sapply(1:length(get.names), function(x){
        if (length(get.names[[x]]) ==1) get.names[[x]]
        else paste(get.names[[x]][1], get.names[[x]][2], sep=":")
    })
    # p-values
    pVal <- 1 - pchisq((beta/se)^2, 1)
    if (length(digits)==1) digits <- rep(digits,2)
    ci.defaults <- list(format="[l;u]",digits=digits[[1]],nsmall=digits[[1]],degenerated="asis")
    pvalue.defaults <- list(digits=digits[[2]],eps=10^{-digits[[2]]},stars=FALSE)
    Lower <- exp(beta - z * se)
    Upper <- exp(beta + z * se)
    smartF <- prodlim::SmartControl(call=list(...),
                                    keys=c("pvalue"),
                                    ignore=c("object","print","handler","digits","nsmall"),
                                    defaults=list("pvalue"=pvalue.defaults),
                                    forced=list("ci"=list(lower=Lower,upper=Upper,handler=handler,digits=digits[[1]],nsmall=nsmall[[1]]),"pvalue"=list(x=pVal)),
                                    verbose=FALSE)
    out <- data.frame("Var"=covNames,
                      "HR"=pubformat(exp(beta),handler=handler,digits=digits,nsmall=digits),
                      "Std.err"=pubformat(format(se,handler=handler,digits=digits,nsmall=digits)),
                      "CI.95"=do.call("formatCI",smartF$ci),
                      "p-value"=do.call("format.pval",smartF$pvalue))
    rownames(out) <- rep("",NROW(out))
    if (print==TRUE)
        publish(out,...)
}


