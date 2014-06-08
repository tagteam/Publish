##' @S3method publish prodlim
publish.prodlim <- function(object,times,intervals=TRUE,percent=TRUE,digits=ifelse(percent,1,3),cause=1,...){
    if (missing(times)) stop("Argument times is missing with no default.")
    so <- summary(object,times=times,intervals=intervals,percent=percent,cause=cause)$table
    if (!is.list(so)) so <- list(so)
    if (object$model=="competing.risks"){
        so <- so[[1]]
    }
    if (length(so)==1) {
        so <- so[[1]]
        if (!is.null(object$cluster)){
            names <- sapply(c("n.risk","n.event","n.lost"),function(x)grep(x,colnames(so),val=TRUE))
            out <- so[,names]}
        else{
            out <- so[,c("n.risk","n.event","n.lost")]
            colnames(out) <- c("No. at risk","No. of events","No. lost to follow-up")
        }
        if (is.null(object$cuminc)){
            out <- cbind(out,"Survival probability"=format(so[,"surv"],digits=digits,nsmall=digits))
        }
        else{
            out <- cbind(out,"Cumulative incidence"=format(so[,"cuminc"],digits=digits,nsmall=digits))
        }
        out <- cbind("Interval"=apply(format(so[,c("time0","time1")],digits=digits,nsmall=digits),1,paste,collapse="--"),
                     out,
                     "CI.95"=apply(format(so[,c("lower","upper")],digits=digits,nsmall=digits),1,paste,collapse="--"))
        publish(out,rownames=FALSE,...)
        invisible(out)
    }
    else{
        names <- names(so)
        u <- lapply(1:length(so),function(i){
            x <- so[[i]]
            out <- x[,c("n.risk","n.event","n.lost"),drop=FALSE]
            colnames(out) <- c("No. at risk","No. of events","No. lost to follow-up")
            if (is.null(object$cuminc)){
                out <- cbind(out,"Survival probability"=format(x[,"surv"],digits=digits,nsmall=digits))
            }
            else{
                out <- cbind(out,"Cumulative incidence"=format(x[,"cuminc"],digits=digits,nsmall=digits))
            }
            out <- cbind("Interval"=apply(format(x[,c("time0","time1"),drop=FALSE],digits=digits,nsmall=digits),1,paste,collapse="--"),
                         out,
                         "CI.95"=apply(format(x[,c("lower","upper"),drop=FALSE],digits=digits,nsmall=digits),1,paste,collapse="--"))
            publish(names[i])
            publish(out,rownames=FALSE,...)
            out})
        if (all(sapply(u,NROW)==1)){
            u.out <- do.call("rbind",u)
            rownames(u.out) <- names(so)
        }else{
            names(u) <- names(so)
            u.out <- u
        }
        invisible(u.out)
    }
}
