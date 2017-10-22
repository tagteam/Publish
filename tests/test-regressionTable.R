### test-regressionTable.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Aug 13 2017 (07:39) 
## Version: 
## Last-Updated: Oct 22 2017 (16:43) 
##           By: Thomas Alexander Gerds
##     Update #: 5
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
data(Diabetes)

test_that("regressiontable: transformed variables and factor levels",{
    Diabetes$hyp1 <- factor(1*(Diabetes$bp.1s>140))
    Diabetes$ofak <- ordered(sample(letters[1:11],size=NROW(Diabetes),replace=1L))
    levels(Diabetes$frame) <- c("+large","medi()um=.<",">8")
    f <- glm(hyp1~frame+gender+log(age)+I(chol>245)+ofak,data=Diabetes,family="binomial")
    regressionTable(f)
    summary(regressionTable(f))
})

test_that("plot.regressionTable",{
    Diabetes$hyp1 <- factor(1*(Diabetes$bp.1s>140))
    Diabetes$ofak <- ordered(sample(letters[1:11],size=NROW(Diabetes),replace=1L))
    levels(Diabetes$frame) <- c("+large","medi()um=.<",">8")
    f <- glm(hyp1~frame+gender+log(age)+I(chol>245)+ofak,data=Diabetes,family="binomial")
    f <- glm(hyp1~log(age)+I(chol>245),data=Diabetes,family="binomial")
    u <- regressionTable(f)
    plot(u,xlim=c(0.5,3))
})

######################################################################
### test-regressionTable.R ends here
