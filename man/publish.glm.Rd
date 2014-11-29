\name{publish.glm}
\alias{publish.glm}
\title{Tabulize regression coefficients with confidence intervals and p-values.}
\usage{
\method{publish}{glm}(object, digits = 2, pvalue.digits = 4, eps = 0.0001,
  pvalue.stars = FALSE, showMissing = NULL, output.columns = NULL,
  intercept = 0, print = TRUE, transform = NULL, profile = TRUE,
  ci.format = NULL, reference = "extraline", ...)
}
\arguments{
  \item{object}{A \code{glm} object.}

  \item{digits}{Rounding digits for all numbers but the
  p-values.}

  \item{pvalue.digits}{Rounding digits for p-values.}

  \item{eps}{passed to format.pval}

  \item{pvalue.stars}{If \code{TRUE} significance is
  indicated by stars instead of p-values.}

  \item{showMissing}{If \code{TRUE} show number of missing
  values in table}

  \item{output.columns}{Select which parameters of the
  result are shown in the table. Defaults to
  \code{c("Coefficient","CI.95","pValue","Missing")} for
  linear regression and to
  \code{c("OddsRatio","CI.95","pValue","Missing")} for
  logistic regression. Can also include
  \code{StandardError}.}

  \item{intercept}{If \code{FALSE} suppress intercept}

  \item{print}{If \code{FALSE} do not print results.}

  \item{transform}{Transformation for regression
  coefficients.}

  \item{profile}{For logistic regression only. If
  \code{FALSE} run with Wald confidence intervals instead
  of waiting for profile confidence intervals.}

  \item{ci.format}{Format for confidence intervals passed
  to \code{\link{sprintf}} with two arguments: the lower
  and the upper limit.}

  \item{reference}{Style for showing results for
  categorical variables. If \code{"extraline"} show an
  additional line for the reference category.}

  \item{...}{passed to labelUnits}
}
\value{
Table with regression coefficients, confidence intervals
and p-values.
}
\description{
Tabulize the part of the result of a generalized linear
regression analysis which is commonly shown in
publications.
}
\details{
For logistic regression, i.e. when family = binomial,
return odds ratio's with confidence intervals.
}
\examples{
data(Diabetes)
f = glm(bp.2s~frame+gender+age,data=Diabetes)
publish(f)
publish(f,reference="inline")
publish(f,pvalue.stars=TRUE)
publish(f,ci.format="(\%1.1f,\%1.1f)")
}
\author{
Thomas Alexander Gerds
}

