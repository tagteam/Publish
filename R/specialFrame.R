specialFrame <- function(call,special,...){
    ## first two args must be
    ## formula and data
    ## the formula may or may not specify special
    m <- call[1:3]
    Terms <- terms(x=eval(m$formula), specials=special, data = eval(m$data))
    m$formula <- Terms
    m[[1]] <- as.name("model.frame")
    m <- eval(m, parent.frame())
    design <- prodlim::model.design(data=m,...)
    ## max.order=1,
    ## drop.intercept=TRUE,
    ## specials.to.factor="strata")
    Y <- model.extract(m, "response")
    ## if (NCOL(Y)>1) stop("Response must be one-dimensional.")
    Y <- data.frame(Y)
    colnames(Y) <- "response"
    out <- c(Y,design)
    class(out) <- "SpecialFrame"
    out
}
