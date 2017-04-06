### test-publish.R --- 
#----------------------------------------------------------------------
## author: Brice Ozenne
## created: apr  6 2017 (10:04) 
## Version: 
## last-updated: apr  6 2017 (10:12) 
##           By: Brice Ozenne
##     Update #: 2
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:



#----------------------------------------------------------------------
### test-publish.R ends here

library(testthat)

context("publish: gls regression")

## simulation
library(nlme)
library(lava)
m <- lvm(Y ~ X1 + gender + group + Interaction)
distribution(m, ~gender) <- binomial.lvm()
distribution(m, ~group) <- binomial.lvm(size = 2)
constrain(m, Interaction ~ gender + group) <- function(x){x[,1]*x[,2]}
d <- sim(m, 1e2)
d$gender <- factor(d$gender, labels = letters[1:2])
d$group <- factor(d$group)

## model
testthat("publish matches gls", {
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
