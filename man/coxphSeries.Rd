\name{coxphSeries}
\alias{coxphSeries}
\title{Run a series of Cox regression models}
\usage{
  coxphSeries(formula, data, vars, ...)
}
\arguments{
  \item{formula}{The fixed part of the regression formula.
  For univariate analyses this is simply
  \code{Surv(time,status)~1} where \code{Surv(time,status)}
  is the outcome variable. When the aim is to control the
  effect of \code{vars} in each element of the series by a
  fixed set of variables it is
  \code{Surv(time,status)~x1+x2} where again
  Surv(time,status) is the outcome and x1 and x2 are
  confounders.}

  \item{data}{A \code{data.frame} in which the
  \code{formula} gets evaluated.}

  \item{vars}{A list of variable names, the changing part
  of the regression formula.}

  \item{...}{passed to publish.coxph}
}
\value{
  matrix with results
}
\description{
  Run a series of Cox regression analyses and summarize the
  results in a table
}
\details{
  This function runs on \code{coxph} from the survival
  package.
}
\author{
  Thomas Alexander Gerds
}

