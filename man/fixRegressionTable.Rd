\name{fixRegressionTable}
\alias{fixRegressionTable}
\title{Expand regression coefficients}
\usage{
fixRegressionTable(x, varnames, reference.value, reference.style = NULL,
  factorlevels, scale, nmiss, intercept)
}
\arguments{
  \item{x}{object to be fixed}

  \item{varnames}{Names of variables}

  \item{reference.value}{Reference value for reference
  categories}

  \item{reference.style}{Style for showing results for
  categorical variables. If \code{"extraline"} show an
  additional line for the reference category.}

  \item{factorlevels}{Levels of the categorical variables.}

  \item{scale}{Scale for some or all of the variables}

  \item{nmiss}{Number of missing values}

  \item{intercept}{Intercept}
}
\value{
a new table with regression coefficients
}
\description{
Expand tables showing regression coefficients
}
\details{
For factors a reference line is added. Shown are numbers of
missing values, units and for log transformed variables the
scale.
}
\author{
Thomas Alexander Gerds
}

