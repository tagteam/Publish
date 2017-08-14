### test-univariateTable.R --- 
#----------------------------------------------------------------------
## author: Thomas Alexander Gerds
## created: May  9 2015 (07:55) 
## Version: 
## last-updated: May  9 2015 (08:42) 
##           By: Thomas Alexander Gerds
##     Update #: 3
#----------------------------------------------------------------------
## 
### Commentary: 
## 
### Change Log:
#----------------------------------------------------------------------
## 
### Code:

if (!library(prodlim,logical.return = TRUE))
{install.packages("prodlim");library(prodlim)}
library(Publish)
source("http://192.38.117.59/~tag/download/univariateTable.R")

Diabetes <- read.csv("http://192.38.117.59/~tag/Teaching/share/data/Diabetes.csv")
Diabetes$AgeGroups <- cut(Diabetes$age,
                          c(19,29,39,49,59,69,92),
                          include.lowest=TRUE)
Diabetes$AgeGroups2 <- as.numeric(Diabetes$AgeGroups)

univariateTable(~age +gender + height + weight,data=Diabetes)

publish(summary(univariateTable(~age+gender+ height+weight,data=Diabetes)),org=TRUE)

univariateTable(location~age+gender+height+weight,data=Diabetes)

publish(summary(univariateTable(location~age+gender+height+weight,
                                data=Diabetes)),org=TRUE)

Diabetes$AgeGroups <- cut(Diabetes$age,c(19,29,39,49,59,69,92),include.lowest=TRUE)
univariateTable(location~AgeGroups+gender+height+weight,data=Diabetes)

publish(summary(univariateTable(location~AgeGroups+gender+height+weight,data=Diabetes)),org=TRUE)

Diabetes$AgeGroups2 <- as.numeric(Diabetes$AgeGroups)
univariateTable(location~AgeGroups2+gender+height+weight,data=Diabetes)

publish(summary(univariateTable(location~AgeGroups2+gender+height+weight,data=Diabetes)),org=TRUE)


f<- location~factor(AgeGroups2)+gender+height+weight
univariateTable(location~factor(AgeGroups2)+gender+height+weight,data=Diabetes)

publish(summary(univariateTable(location~factor(AgeGroups2)+gender+height+weight,data=Diabetes)),org=TRUE)

v <- univariateTable(gender ~age+height,data=Diabetes)
sv <- summary(v,missing="always")

v <- univariateTable(gender ~age+height,data=Diabetes)
publish(summary(v,missing="always"),org=TRUE)

univariateTable(location~factor(AgeGroups2)+gender+height+weight,
                data=Diabetes,
                summary.format="median(x) {iqr(x)}")

publish(summary(univariateTable(location~factor(AgeGroups2)+gender+height+weight,data=Diabetes,summary.format="median(x) {iqr(x)}")),org=TRUE)

univariateTable(location~age+height+weight,
                data=Diabetes,
                summary.format="Mean: mean(x) SD: sd(x) Median: median(x) IQR=[iqr(x)] Range: [minmax(x)]")

ux <- univariateTable(location~age+height+weight,
              data=Diabetes,
              summary.format="Mean: mean(x) SD: sd(x) Median: median(x) IQR=[iqr(x)] Range: [minmax(x)]")
sux <- summary(ux)
publish(sux,org=TRUE)

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
publish(univariateTable(location~age+height+weight,data=Diabetes,summary.format="MeanSe(x)"),org=TRUE)

publish(summary(univariateTable(location~age,
                                data=Diabetes,
                                summary.format="MeanSe(x)")),
        org=TRUE)

ux <- univariateTable(location~gender+age+AgeGroups,
                      data=Diabetes,
                      column.percent=FALSE,
                      freq.format="count(x)")
sux <- summary(ux)
publish(sux,org=TRUE)

ux <- univariateTable(location~gender+age+AgeGroups,
                      data=Diabetes,
                      freq.format="percent(x)",
                      column.percent=FALSE)
sux <- summary(ux)
publish(sux,org=TRUE)

ux <- univariateTable(location~gender+age+AgeGroups,
                      data=Diabetes,
                      column.percent=TRUE,
                      freq.format="count(x) (percent(x))")
sux <- summary(ux)
publish(sux,org=TRUE)

ux <- univariateTable(location~gender+age+AgeGroups,
                      data=Diabetes,
                      column.percent=TRUE,
                      freq.format="percent(x)")
sux <- summary(ux)
publish(sux,org=TRUE)

univariateTable(location~age+gender+height+weight,data=Diabetes,summary.format="median(x) [iqr(x)]",digits.summary=3)

publish(summary(univariateTable(location~age+gender+height+weight,data=Diabetes,summary.format="median(x) [iqr(x)]",digits.freq=3,digits.summary=3)),org=TRUE)

table1 <- summary(univariateTable(location~age+gender+height+weight,data=Diabetes,summary.format="median(x) [iqr(x)]",digits.summary=3))
table1 <- table1[,-5]
table1

publish(table1,org=TRUE)

table1 <- summary(univariateTable(location~age+gender+height+weight,data=Diabetes,summary.format="median(x) [iqr(x)]",digits.summary=3))
colnames(table1) <- gsub("location = ","",colnames(table1))
table1[,1] <- toupper(unlist(table1[,1]))
table1

publish(table1,org=TRUE)

publish(table1,org=TRUE)

publish(table1,latex=TRUE)




#----------------------------------------------------------------------
### test-univariateTable.R ends here
