print.regressionTable <- function(x,...){
    Rtab <- summary(x,print=FALSE,...)
    ## rownames(Rtab) <- NULL
    print.listof(Rtab,...)
    Rtab
}
