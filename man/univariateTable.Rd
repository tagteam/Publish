\name{univariateTable}
\alias{univariateTable}
\title{Univariate table}
\usage{
univariateTable(formula, data = parent.frame(),
  summary.format = "mean(x) (sd(x))", Q.format = "median(x) [iqr(x)]",
  freq.format = "count(x) (percent(x))", column.percent = TRUE,
  digits.summary = 1, digits.freq = 1, strataIsOutcome = FALSE,
  shortGroupNames, ...)
}
\arguments{
  \item{formula}{}

  \item{data}{}

  \item{summary.format}{}

  \item{Q.format}{}

  \item{freq.format}{}

  \item{column.percent}{Logical, if \code{TRUE} and the
  default freq.format is used then column percentages are
  given instead of row percentages for discrete factors.}

  \item{digits.summary}{}

  \item{digits.freq}{}

  \item{strataIsOutcome}{}

  \item{shortGroupNames}{}

  \item{...}{Not yet used}
}
\value{
List
}
\description{
##' ##' ##' Categorical variables are summarized using
counts and frequencies. Continuous variables are summarized
by means and standard deviations. Deviations from the above
defaults are obtained when the arguments summary.format and
freq.format are combined with suitable summary functions.
}
\examples{
data(Diabetes)
univariateTable(~age+gender+ height+weight,data=Diabetes)
univariateTable(location~age+gender+height+weight,data=Diabetes)

## Use quantiles and rank tests for some variables and mean and standard deviation for others
univariateTable(gender~Q(age)+location+Q(BMI)+height+weight,data=Diabetes)

## Factor with more than 2 levels
Diabetes$AgeGroups <- cut(Diabetes$age,c(19,29,39,49,59,69,92),include.lowest=TRUE)
univariateTable(location~AgeGroups+gender+height+weight,data=Diabetes)

## Column percent
univariateTable(location~gender+age+AgeGroups, data=Diabetes,freq.format="count(x) (colpercent(x))")

## Labels
u <- univariateTable(location~gender+AgeGroups+ height + weight, data=Diabetes,freq.format="count(x) (colpercent(x))")
summary(u,"AgeGroups"="Age (years)","height"="Height (inches)")
}
\author{
Thomas A. Gerds
}
\seealso{
summary.univariateTable, publish.univariateTable
}

