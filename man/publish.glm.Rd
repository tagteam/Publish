\name{publish.glm}
\alias{publish.glm}
\title{Tabulize regression coefficients with confidence intervals and p-values.}
\usage{
\method{publish}{glm}(object, digits = 2, pvalue.digits = 4, eps = 0.0001,
  pvalue.stars = FALSE, showMissing = NULL, output.columns = NULL,
  intercept = 0, print = TRUE, transform = NULL, profile = TRUE,
  ci.format, style = "extraline", ...)
}
\arguments{
  \item{object}{}

  \item{digits}{}

  \item{pvalue.digits}{}

  \item{eps}{}

  \item{pvalue.stars}{}

  \item{showMissing}{}

  \item{output.columns}{Select which parameters of the
  result are shown in the table. Defaults to
  \code{c("Coefficient","CI.95","pValue","Missing")} for
  linear regression and to
  \code{c("OddsRatio","CI.95","pValue","Missing")} for
  logistic regression. Can also include
  \code{StandardError}.}

  \item{intercept}{}

  \item{print}{}

  \item{transform}{}

  \item{profile}{}

  \item{ci.format}{}

  \item{style}{}

  \item{...}{}
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
\author{
Thomas Alexander Gerds
}

