print.regressionTable <- function(x,...){
    Rtab <- summary(x,...)
    ## rownames(Rtab) <- NULL
    ## print(Rtab,...)
    Rtab
}
