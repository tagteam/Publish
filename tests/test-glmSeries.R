### test-glmSeries.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Feb 10 2018 (12:44) 
## Version: 
## Last-Updated: Feb 10 2018 (18:37) 
##           By: Thomas Alexander Gerds
##     Update #: 2
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:
library(testthat)
library(Publish)
library(data.table)
data(Diabetes)

test_that("glmSeries missing data, data.table ",{
    Diabetes <- as.data.frame(Diabetes)
    Diabetes$hypertension <- factor(Diabetes$bp.1s>140)
    a <- glmSeries(vars=c("bp.2s","frame","weight","age"),formula=hypertension~gender,data=Diabetes,family=binomial)
    expect_equal(a$Missing,c(262,12,"","","1","0"))
    setDT(Diabetes)
    b <- glmSeries(vars=c("bp.2s","frame","weight","age"),formula=hypertension~gender,data=Diabetes,family=binomial)
    expect_equal(a,b)
})



######################################################################
### test-glmSeries.R ends here
