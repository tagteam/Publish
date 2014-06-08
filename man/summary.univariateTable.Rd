\name{summary.univariateTable}
\alias{summary.univariateTable}
\title{Preparing univariate tables for publication}
\usage{
  summary.univariateTable(x,
    missing = c("ifany", "always", "never"), n = "inNames",
    pvalue.stars = FALSE, pvalue.eps = 0.0001,
    pvalue.digits = 4, ...)
}
\arguments{
  \item{x}{\code{univariateTable} object as obtained with
  function \code{univariateTable}.}

  \item{missing}{Decides if number of missing values are
  shown in table. Defaults to \code{"ifany"}, and can also
  be set to \code{"always"} or \code{"never"}.}

  \item{pvalue.stars}{If TRUE use \code{symnum} to parse
  p-values otherwise use \code{format.pval}.}

  \item{pvalue.eps}{Passed to \code{format.pval}.}

  \item{pvalue.digits}{Passed to \code{format.pval}.}

  \item{...}{}
}
\value{
  Summary table
}
\description{
  Summary function for univariate table
}
\details{
  Collects results of univariate table in a matrix.
}
\author{
  Thomas A. Gerds
}

