##' @export
publish.prodlim <- function(object,times,intervals=TRUE,percent=TRUE,digits=ifelse(percent,1,3),cause=1,surv=TRUE,print=TRUE,...){
    if (missing(times)) stop("Argument times is missing with no default.")
    so <- summary(object,times=times,intervals=intervals,percent=percent,cause=cause,surv=surv)
    data.table::setDT(so)
    if (match("cuminc",colnames(so),nomatch=FALSE)==0){
        nn = "surv"
        se = "se.surv"
        nn = "Survival probability"
        data.table::set(so,j = "Survival probability",value = format(so[["surv"]],digits=digits,nsmall=digits))
    } else{
        nn = "cuminc"
        se = "se.cuminc"
        NN = "Absolute risk"
        data.table::set(so,j = "Absolute risk",value = format(so[["cuminc"]],digits=digits,nsmall=digits))
    }
    data.table::set(so,j = "Interval", value = apply(format(so[,c("time0","time1"),drop=FALSE],digits=digits,nsmall=digits),1,paste,collapse="--"))
    data.table::set(so,j = "CI.95", value = formatCI(lower = so[["lower"]],upper = so[["upper"]],digits=digits,nsmall=digits))
    for (n in c("time0","time1","lower","upper",nn,se)) data.table::set(so,j = n,value = NULL)
    vv = c("Interval",NN,"CI.95")
    not_vv = setdiff(names(so),vv)
    data.table::setcolorder(so,c(not_vv,vv))
    if (print==TRUE){
        publish(so,rownames=FALSE,...)
    }
    invisible(so)
}
