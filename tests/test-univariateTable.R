### test-univariateTable.R --- 
#----------------------------------------------------------------------
## author: Thomas Alexander Gerds
## created: May  9 2015 (07:55) 
## Version: 
## last-updated: Oct 22 2017 (17:33) 
##           By: Thomas Alexander Gerds
##     Update #: 8
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

library(testthat)
library(prodlim)
library(Publish)
data(Diabetes)

test_that("univariateTable no groups",{
    u1 <- univariateTable(~age +gender + height + weight,data=Diabetes)
    a <- summary(u1,show.missing=1L)
    expect_equal(NROW(a),9)
    b <- summary(u1,show.missing=0L)
    expect_equal(NROW(b),5)
    u2 <- univariateTable(~age,data=Diabetes)
    u3 <- univariateTable(~gender,data=Diabetes)
    a1 <- publish(univariateTable(~age+gender+ height+weight,data=Diabetes))
    a2 <- publish(summary(univariateTable(~age+gender+ height+weight,data=Diabetes)))
    expect_equal(a1,a2)
})

test_that("Univariate table with groups and missing values and labels with special characters",{
    Diabetes$AgeGroups <- cut(Diabetes$age,
                              c(19,29,39,49,59,69,92),
                              include.lowest=TRUE)
    univariateTable(location~age+gender+height+weight+AgeGroups,data=Diabetes)
    publish(summary(univariateTable(location~age+gender+height+weight,
                                    data=Diabetes)),org=TRUE)
    v <- univariateTable(gender ~age+height,data=Diabetes)
    sv <- summary(v,show.missing="always")
    univariateTable(location~factor(AgeGroups)+gender+height+weight,
                    data=Diabetes,
                    summary.format="median(x) {iqr(x)}")
    levels(Diabetes$frame) <- c("+large","medi()um=.<",">8")
    expect_output(publish(summary(univariateTable(frame~age+gender+height+weight+location,
                                                  data=Diabetes)),org=TRUE))    
    expect_output(publish(summary(univariateTable(location~age+gender+height+weight+frame,
                                                  data=Diabetes)),org=TRUE))
})

test_that("Univariate table with row percent",{
    a <- summary(univariateTable(frame~gender+location, data=Diabetes,column.percent=TRUE))
    b <- summary(univariateTable(frame~gender+location, data=Diabetes,column.percent=FALSE))
    expect_equal(as.numeric(colSums(a[a$Variable=="gender"]==b[b$Variable=="gender"])),c(4,0))
})

if (FALSE){
test_that("Univariate table with stupid function",{
    stupid <- function(x){
        if (mean(x)>47) "large" else "small"
    }
    univariateTable(location~age+height+weight,
                    data=Diabetes,
                    summary.format="Mean: mean(x) stupid's distance: (stupid(x))")

    publish(summary(univariateTable(location~age+height+weight,
                                    data=Diabetes,
                                    summary.format="Mean: mean(x) stupid's distance: (stupid(x))")),
            org=TRUE)
    MeanSe <- function(x){
        paste("Mean=",round(mean(x),1)," Standard.error=",round(sd(x)/sqrt(length(x)),3),sep="")
    }
    expect_output(publish(univariateTable(location~age+height+weight,data=Diabetes,summary.format="MeanSe(x)")))
    ux <- univariateTable(location~gender+age+AgeGroups,
                          data=Diabetes,
                          column.percent=FALSE,
                          freq.format="count(x)")
    sux <- summary(ux)
    publish(sux,org=TRUE)
})
}
#----------------------------------------------------------------------
### test-univariateTable.R ends here
