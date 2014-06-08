\name{glmSeries}
\alias{glmSeries}
\title{Run a series of generalized linear regression analyses}
\usage{
  glmSeries(formula, data, vars, ...)
}
\arguments{
  \item{formula}{The fixed part of the regression formula.
  For univariate analyses this is simply \code{y~1} where
  \code{y} is the outcome variable. When the aim is to
  control the effect of \code{vars} in each element of the
  series by a fixed set of variables it is \code{y~x1+x2}
  where again y is the outcome and x1 and x2 are
  confounders.}

  \item{data}{A \code{data.frame} in which we evaluate the
  formula.}

  \item{vars}{A list of variable names, the changing part
  of the regression formula.}

  \item{...}{passed to coxph}
}
\value{
  Matrix with regression coefficients, one for each element
  of \code{vars}.
}
\description{
  Run a series of generalized linear regression analyses
  and summarize the results in a table
}
\author{
  Thomas Alexander Gerds

  ##'
}

