plot.glm <- function(x,...){
    ctable <- data.frame(coef(summary(x)))[-1,]
    ciX <- suppressMessages(confint(x))[-1,]
    upper <- exp(ciX[,2])
    plot.ci(exp(ctable[,,drop=FALSE]),
            plot.xlim=c(0,max(upper)),
            labels=rownames(ctable),
            title.labels="Factor",
            lower=exp(ciX[,1]),
            upper=upper,...)
}
## plot(logreg)

