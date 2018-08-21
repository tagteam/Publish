update.label <- function(str,low=NULL,upper=NULL,low.str="%l",upper.str="%u"){
    if(is.null(low)) low <- low.str
    if(is.null(upper)) upper <- upper.str
    new.label <- str
    new.label <- sub(low.str, low, new.label)
    new.label <- sub(upper.str, upper, new.label)
    return(new.label)
}

acut <- function(x,n=5,format=NULL,format.low=NULL,format.high=NULL,dig.lab=3,...){
    stopifnot(n>1)
    breaks <- as.numeric(quantile(x, seq(0,1,length.out=n+1)))
    breaks[1] <- -Inf
    breaks[length(breaks)] <- Inf
    out <- cut(x,breaks=breaks,...)
    if(!is.null(c(format,format.low,format.high))){
        out.labels <- levels(out)
        if(!is.null(format))
            out.labels <- mapply(
                function(a,b) update.label(format,low=a,upper=b),
                round(breaks[1:(length(breaks)-1)],digits=dig.lab),
                round(breaks[2:(length(breaks))],digits=dig.lab)
            )
        if(!is.null(format.low))
            out.labels[1] <- update.label(format.low,
                                          low=round(breaks[1],digits=dig.lab),
                                          upper=round(breaks[2],digits=dig.lab))
        if(!is.null(format.high))
            out.labels[length(out.labels)] <- update.label(format.high,
                                                           low=round(breaks[length(breaks)-1],digits=dig.lab),
                                                           upper=round(breaks[length(breaks)],digits=dig.lab))
        levels(out) <- out.labels
    }
    return(out)
}
