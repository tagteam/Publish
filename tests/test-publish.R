### test-publish.R --- 
#----------------------------------------------------------------------
## author: Brice Ozenne
## created: apr  6 2017 (10:04) 
## Version: 
## last-updated: Aug 14 2017 (19:29) 
##           By: Thomas Alexander Gerds
##     Update #: 10
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
context("publish: default and matrix")

test_that("publish rounding of a matrix with NA", {
    set.seed(7)
    y0 <- cbind(a=rnorm(2),b=1:2,c=letters[1:2])
    y1 <- y0
    y1[1,1] <- NA
    y1[2,2] <- NA
    b <- publish(y1,digits=1)
    expect_equal(c(b),c("  NA","-1.2","1.0"," NA","a","b"))
})


#----------------------------------------------------------------------
### test-publish.R ends here
