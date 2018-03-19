library(testthat)
library(Publish)
library(mitools)
library(smcfcs)
library(riskRegression)

test_that("multiple imputation",{
    set.seed(71)
    d=sampleData(100)
    ## generate missing values
    d[X1==1,X6:=NA]
    d[X2==1,X3:=NA]
    d=d[,.(X8,X4,X3,X6,X7)]
    sapply(d,function(x)sum(is.na(x)))
    d[,X4:=factor(X4,levels=c("0","1"),labels=c("0","1"))]
    set.seed(17)
    f= smcfcs(d,smtype="lm",smformula=X8~X4*X3+X6+X7,method=c("","","logreg","norm",""),m=3)
    ccfit=lm(X8~X4*X3+X6+X7,data=d)
    impobj <- imputationList(f$impDatasets)
    models <- with(impobj,lm(X8~X4*X3+X6+X7))
    mifit <- MIcombine(models)
    a <- publish(mifit,fit=ccfit,data=d)
})
