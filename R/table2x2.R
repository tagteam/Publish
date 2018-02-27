##' 2x2 table calculus for teaching
##'
##' 2x2 table calculus for teaching
##' @title 2x2 table calculus for teaching
##' @param x 2x2 table
##' @param digits rounding digits
##' @param stats subset or all of \code{c("table","rd","or","rr","chisq","fisher")} where rd= risk difference, rr = risk ratio, or = odds ratio, chisq = chi-square test, fisher= fisher's exact test and table = the 2x2 table
##' @return see example
##' @examples
##' table2x2(table("marker"=rbinom(100,1,0.4),"response"=rbinom(100,1,0.1)))
##' table2x2(matrix(c(71,18,38,8),ncol=2),stats="table")
##' table2x2(matrix(c(71,18,38,8),ncol=2),stats=c("rr","fisher"))
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
table2x2 <- function(x,digits=1,stats=c("table","rd","rr","or","chisq","fisher")){
    if (class(x)=="data.frame"){
        table2x2 <- as.matrix(x)
    } else{
        if (class(x)=="matrix"||class(x)=="table"){
            if (class(x)=="table"){table2x2 <- as.matrix(x)}
            else table2x2 <- x
        } else{
            stop("first argument `x' must be a matrix or a data.frame")
        }
    }
    if (NROW(x)!=2) stop("Matrix must have exactly 2 rows")
    if (NCOL(x)!=2) stop("Matrix must have exactly 2 columns")
    a <- table2x2[1,1]
    b <- table2x2[1,2]
    c <- table2x2[2,1]
    d <- table2x2[2,2]
    p1 <- a/(a+b)
    p2 <- c/(c+d)
    ## ## test statistic 
    ## n <- (a+b+c+d)
    ## chi2test <- (a*d-b*c)^2*n/((a+c)*(b+d)*(a+b)*(c+d))
    ## 2x2 table
    out <- list(table2x2=table2x2,stats=stats)
    if ("rd" %in% stats){
        rd <- (p1-p2)
        se.rd <- sqrt(p1*(1-p1)/(a+b)+p2*(1-p2)/(c+d))
        rd.lower <- rd - qnorm(1-0.05/2)*se.rd
        rd.upper <- rd + qnorm(1-0.05/2)*se.rd
        out <- c(out,list(rd=rd,se.rd=rd,rd.lower=rd.lower,rd.upper=rd.upper))
    }
    if ("rr" %in% stats){
        rr <- p1/p2
        se.rr <- sqrt((1-p1)/a+(1-p2)/c)
        rr.lower <- rr * exp(- qnorm(1-0.05/2) * se.rr)
        rr.upper <- rr * exp(  qnorm(1-0.05/2) * se.rr)
        out <- c(out,list(rr=rr,se.rr=rr,rr.lower=rr.lower,rr.upper=rr.upper))
    }
    if ("or" %in% stats){
        or <- (a*d)/(b*c)
        se.or <- sqrt(1/a+1/b+1/c+1/d)
        or.lower <- exp(log(or) - qnorm(1-0.05/2)*se.or)
        or.upper <- exp(log(or) + qnorm(1-0.05/2)*se.or)
        out <- c(out,list(or=or,se.or=se.or,or.lower=or.lower,or.upper=or.upper))
    }
    class(out) <- "table2x2"
    out
}
