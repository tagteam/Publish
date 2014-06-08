\name{publish.coxph}
\alias{publish.coxph}
\title{Tabulize hazard ratios with confidence intervals and p-values.}
\usage{
\method{publish}{coxph}(object, digits = 2, pvalue.digits = 4,
  eps = 0.0001, pvalue.stars = FALSE, showMissing = "ifany",
  output.columns = NULL, print = TRUE, ci.format = paste("[",
  paste("\%1.", digits, "f", sep = ""), ";", paste("\%1.", digits, "f", sep =
  ""), "]", sep = ""), style = "extraline", ...)
}
\arguments{
  \item{object}{A \code{coxph} object.}

  \item{digits}{}

  \item{pvalue.digits}{}

  \item{eps}{}

  \item{pvalue.stars}{}

  \item{showMissing}{}

  \item{output.columns}{}

  \item{print}{}

  \item{ci.format}{}

  \item{style}{}

  \item{...}{}

  \item{scale}{Scaling factor multiplied to both the
  log-hazard ratio and its standard-error. Useful to change
  the units of the intepretation scale.}
}
\value{
Table with hazard ratios, confidence intervals and
p-values.
}
\description{
Tabulize the part of the result of a Cox regression
analysis which is commonly shown in publications.
}
\details{
Transforms the log hazard ratios to hazard ratios and
returns them with confidence limits and p-values.
}
\author{
Thomas Alexander Gerds
}

