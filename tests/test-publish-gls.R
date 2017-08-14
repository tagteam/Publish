### test-publish-gls.R --- 
#----------------------------------------------------------------------
## Author: Thomas Alexander Gerds
## Created: Aug 14 2017 (18:56) 
## Version: 
## Last-Updated: Aug 14 2017 (19:17) 
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
context("publish: gls regression")

## simulation
library(nlme)
library(Publish)
library(lava)
m <- lvm(Y ~ X1 + gender + group + Interaction)
distribution(m, ~gender) <- binomial.lvm()
distribution(m, ~group) <- binomial.lvm(size = 2)
constrain(m, Interaction ~ gender + group) <- function(x){x[,1]*x[,2]}
d <- sim(m, 1e2)
d$gender <- factor(d$gender, labels = letters[1:2])
d$group <- factor(d$group)

## model
test_that("publish matches gls", {
    e.gls <- gls(Y ~ X1 + gender*group, data = d,
                 weights = varIdent(form = ~1|group))
    res <- regressionTable(e.gls)
    Mpublish <- rbind(res$X1,
                      res$gender,
                      res$group)
    Sgls <- summary(e.gls)$tTable
    expect_equal(Mpublish[Mpublish$Coefficient!=0,"Coefficient"],
                 unname(Sgls[c("X1","genderb","group1","group2"),"Value"]))
    expect_equal(Mpublish[Mpublish$Coefficient!=0,"Pvalue"],
                 unname(Sgls[c("X1","genderb","group1","group2"),"p-value"]))
})

context("publish: lme regression")

data("Orthodont")
test_that("publish matches lme", {
    fm1 <- lme(distance ~ age*Sex, 
               random = ~1|Subject,
               data = Orthodont) 
    res <- publish(fm1)
    # main effects
    expect_equal(as.double(res$rawTable[c(1:2,4),"Coefficient"]),
                 as.double(fixef(fm1)[1:3]))
    expect_equal(as.double(res$rawTable[c(1:2,4),"Pvalue"]),
                 as.double(summary(fm1)$tTable[1:3,5]))
    # interaction
    expect_equal(as.double(res$rawTable$Coefficient[6]),
                 as.double(sum(fixef(fm1)[c(2,4)])))
})



######################################################################
### test-publish-gls.R ends here
