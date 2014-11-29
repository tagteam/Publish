\name{publish.coxph}
\alias{publish.coxph}
\title{Tabulize hazard ratios with confidence intervals and p-values.}
\usage{
\method{publish}{coxph}(object, digits = 2, pvalue.digits = 4,
  eps = 0.0001, pvalue.stars = FALSE, ci.format = NULL,
  showMissing = "ifany", output.columns = NULL, print = TRUE,
  reference = "extraline", ...)
}
\arguments{
  \item{object}{A \code{coxph} object.}

  \item{digits}{Rounding digits for all numbers but the
  p-values.}

  \item{pvalue.digits}{Rounding digits for p-values.}

  \item{eps}{passed to format.pval}

  \item{pvalue.stars}{If \code{TRUE} significance is
  indicated by stars instead of p-values.}

  \item{ci.format}{Format for confidence intervals passed
  to \code{\link{sprintf}} with two arguments: the lower
  and the upper limit.}

  \item{showMissing}{If \code{TRUE} show number of missing
  values in table}

  \item{output.columns}{Select output columns}

  \item{print}{If \code{FALSE} do not print results.}

  \item{reference}{Style for showing results for
  categorical variables. If \code{"extraline"} show an
  additional line for the reference category.}

  \item{...}{passed to labelUnits}
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
returns them with confidence limits and p-values. If
explanatory variables are log transformed or log2
transformed, a scaling factor is multiplied to both the
log-hazard ratio and its standard-error.
}
\author{
Thomas Alexander Gerds
}

