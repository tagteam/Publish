#' @export
ci.mean.formula <- function(x,
                            data,
                            alpha = 0.05,
                            normal = TRUE,
                            na.rm=T,
                            statistic=c("arithmetic","geometric"),...){
    work <- model.frame(x,data)
    nf <- ncol(work)-1
    if (nf>1) f <- interaction(work[,-1,drop=FALSE],sep=" - ")
    else f <- factor(work[,2])
    res <- lapply(split(model.response(work),f),ci.mean.default,alpha=alpha,normal=normal,na.rm=na.rm,statistic=statistic)
    statistic <- unique(unlist(lapply(res,function(x)x$statistic)))
    labels <- do.call("rbind",strsplit(names(res)," - "))
    colnames(labels) <- names(work)[-1]
    ##    we reverse the order of factors for nicer labeling ...
    labels <- labels[,rev(1:nf),drop=FALSE]
    res <- data.frame(do.call("rbind",res))
    out <- lapply(res[,1:4],function(x)unlist(x))
    out <- c(out,list(labels=labels,level=alpha,statistic=statistic))
    class(out) <- c("ci",class(out))
    out  
}
