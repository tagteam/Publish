##' print results of 2x2 contingency table analysis
##'
##' @title print results of 2x2 contingency table analysis
##' @param x object obtained with table2x2
##' @param digits rounding digits
##' @param ... not used
##' @return invisible x
##' @seealso table2x2
##' @examples
##' table2x2(table("marker"=rbinom(100,1,0.4),"response"=rbinom(100,1,0.1)))
##' table2x2(matrix(c(71,18,38,8),ncol=2),stats="table")
##' table2x2(matrix(c(71,18,38,8),ncol=2),stats=c("rr","fisher"))
##' @export 
##' @author Thomas A. Gerds <tag@@biostat.ku.dk>
print.table2x2 <- function(x,digits=1,...){
    stats <- x$stats
    table2x2 <- x$table2x2
    a <- table2x2[1,1]
    b <- table2x2[1,2]
    c <- table2x2[2,1]
    d <- table2x2[2,2]
    p1 <- a/(a+b)
    p2 <- c/(c+d)
    if ("table" %in% stats){
        suppressWarnings(X <- data.frame(rbind(table2x2,rep("--",2),table2x2[1,]+table2x2[2,])))
        if (is.null(rownames(table2x2))) rownames(table2x2) <- paste("exposure:",c("no","yes"))
        if (is.null(colnames(table2x2))) colnames(table2x2) <- paste("response:",c("no","yes"))
        X$Sum <- c(a+b,c+d,"--",a+b+c+d)
        colnames(X) <- c(paste(names(attr(table2x2,"dimnames"))[2],attr(table2x2,"dimnames")[[2]],sep=""),"Sum")
        rownames(X) <- c(paste(names(attr(table2x2,"dimnames"))[1],attr(table2x2,"dimnames")[[1]],sep=""),"--","Sum")
        cat("_____________________________\n\n2x2 contingency table\n_____________________________\n\n")    
        print(X,print.gap=5)
        cat("\n_____________________________\n\nStatistics\n_____________________________\n\n")
        cat(paste("\na=",a,"\nb=",b,"\nc=",c,"\nd=",d))
        cat(paste("\n\np1=a/(a+b)=",round(a/(a+b),4),"\np2=c/(c+d)=", round(c/(c+d),4)),"\n")
    }
    if ("rd" %in% stats){
        rd <- x$rd
        se.rd <- x$se.rd
        rd.lower <- x$rd.lower
        rd.upper <- x$rd.upper
        cat(paste("\n_____________________________\n\nRisk difference\n_____________________________\n\n",
                  "Risk difference = RD = p1-p2 = ",
                  format(rd,digits=digits+3,nsmall=digits+3),
                  "\nStandard error = SE.RD = sqrt(p1*(1-p1)/(a+b)+p2*(1-p2)/(c+d)) = ",
                  format(se.rd,digits=digits+3,nsmall=digits+3),
                  "\nLower 95%-confidence limit: = RD - 1.96 * SE.RD = ",
                  format(rd.lower,digits=digits+3,nsmall=digits+3),
                  "\nUpper 95%-confidence limit: = RD + 1.96 * SE.RD = ",
                  format(rd.upper,digits=digits+3,nsmall=digits+3),
                  "\n\nThe estimated risk difference is ",format(100*rd,digits=digits,nsmall=digits),"% ",
                  paste(" (CI_95%: [", format(100*rd.lower,digits=digits,nsmall=digits), ";", format(100*rd.upper,digits=digits,nsmall=digits), "]", sep = ""),
                  ").\n",sep=""))
    }
    if ("rr" %in% stats){
        rr <- x$rr
        se.rr <- x$se.rr
        rr.lower <- x$rr.lower
        rr.upper <- x$rr.upper
        cat(paste("\n_____________________________\n\nRisk ratio\n_____________________________\n\n",
                  "Risk ratio = RR = p1/p2 = ",
                  format(rr,digits=digits+3,nsmall=digits+3),
                  "\nStandard error = SE.RR = sqrt((1-p1)/a+(1-p2)/c)= ",
                  format(se.rr,digits=digits+3,nsmall=digits+3),
                  "\nLower 95%-confidence limit: = RR * exp(- 1.96 * SE.RR) = ",
                  format(rr.lower,digits=digits+3,nsmall=digits+3),
                  "\nUpper 95%-confidence limit: = RR * exp(1.96 * SE.RR) = ",
                  format(rr.upper,digits=digits+3,nsmall=digits+3),
                  "\n\nThe estimated risk ratio is ",format(rr,digits=digits+2,nsmall=digits+2),"",
                  paste(" (CI_95%: [", format(rr.lower,digits=digits+2,nsmall=digits+2), ";", format(rr.upper,digits=digits+2,nsmall=digits+2), "]", sep = ""),
                  ").\n",sep=""))
    }
    if ("or" %in% stats){
        or <- x$or
        se.or <- x$se.or
        or.lower <- x$or.lower
        or.upper <- x$or.upper
        cat(paste("\n_____________________________\n\nOdds ratio\n_____________________________\n\n",
                  "Odds ratio = OR = (p1/(1-p1))/(p2/(1-p2)) = ",
                  format(or,digits=digits+3,nsmall=digits+3),
                  "\nStandard error = SE.OR = sqrt((1/a+1/b+1/c+1/d)) = ",
                  format(se.or,digits=digits+3,nsmall=digits+3),
                  "\nLower 95%-confidence limit: = OR * exp(- 1.96 * SE.OR) = ",
                  format(or.lower,digits=digits+3,nsmall=digits+3),
                  "\nUpper 95%-confidence limit: = OR * exp(1.96 * SE.OR) = ",
                  format(or.upper,digits=digits+3,nsmall=digits+3),
                  "\n\nThe estimated odds ratio is ",format(or,digits=digits+2,nsmall=digits+2),"",
                  paste(" (CI_95%: [", format(or.lower,digits=digits+2,nsmall=digits+2), ";", format(or.upper,digits=digits+2,nsmall=digits+2), "]", sep = ""),
                  ").\n",sep=""))
    }
    if ("chisq" %in% stats){
        cat("\n_____________________________\n\nChi-square test\n_____________________________\n\n")
        print(chisq.test(table2x2))
    }
    if ("fisher" %in% stats){
        cat("\n_____________________________\n\nFisher's exact test\n_____________________________\n\n")
        print(fisher.test(table2x2))
    }
    invisible(x)
}

