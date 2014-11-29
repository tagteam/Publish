\name{publish.CauseSpecificCox}
\alias{publish.CauseSpecificCox}
\title{Tabulizing cause-specific hazard ratio from all causes with confidence limits and Wald test p-values.}
\usage{
\method{publish}{CauseSpecificCox}(object, cause, digits = 2,
  pvalue.digits = 4, eps = 0.0001, pvalue.stars = FALSE,
  ci.format = NULL, showMissing = "ifany", reference = "extraline",
  output.columns = NULL, print = TRUE, ...)
}
\arguments{
  \item{object}{Cause-specific hazard model obtained with
  \code{CSC}.}

  \item{cause}{Show a table for this cause. If omitted,
  list all causes.}

  \item{digits}{Number of digits for all values except
  p-values.}

  \item{pvalue.digits}{Number of digits for p-values}

  \item{eps}{Values smaller than \code{eps} are formatted
  as \code{< eps}.}

  \item{pvalue.stars}{If \code{TRUE}, use star-notation for
  p-values.}

  \item{ci.format}{Format for confidence intervals passed
  to}

  \item{showMissing}{If \code{TRUE} show number of missing
  values per variable.}

  \item{reference}{See \code{publish.coxph}.}

  \item{output.columns}{Pre-select columns.}

  \item{print}{If \code{TRUE} print the table(s).}

  \item{...}{passed to \code{publish.coxph}}
}
\value{
Table with cause-specific hazard ratios, confidence limits
and p-values.
}
\description{
Publish cause-specific Cox models
}
\details{
The cause-specific hazard ratio's are combined into one
table.
}
\examples{
library(riskRegression)
library(prodlim)
library(pec)
library(survival)
data(Melanoma)
 fit1 <- CSC(list(Hist(time,status)~sex,Hist(time,status)~invasion+epicel+age),
            data=Melanoma)
publish(fit1)
publish(fit1,eps=0.001)
publish(fit1,pvalue.stars=TRUE)
publish(fit1,ci.format="(\%1.1f, \%1.1f)")
}
\author{
Thomas Alexander Gerds
}

