### test-regressionTable.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Aug 13 2017 (07:39) 
## Version: 
## Last-Updated: Aug 14 2017 (09:05) 
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
data(Diabetes)

test_that("regressiontable: transformed variables and factor levels",{
    Diabetes$hyp1 <- factor(1*(Diabetes$bp.1s>140))
    Diabetes$ofak <- ordered(sample(letters[1:11],size=NROW(Diabetes),replace=1L))
    levels(Diabetes$frame) <- c("+large","medi()um=.<",">8")
    f <- glm(hyp1~frame+gender+log(age)+I(chol>245)+ofak,data=Diabetes,family="binomial")
    regressionTable(f)
    summary(regressionTable(f))
})

######################################################################
### test-regressionTable.R ends here
