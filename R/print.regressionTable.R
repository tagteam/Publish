print.regressionTable <- function(x,...){
    Rtab <- do.call("rbind",x)
    rownames(Rtab) <- NULL
    print(Rtab,...)
    invisible(Rtab)
}
