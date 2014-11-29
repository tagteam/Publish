\name{lazyFactorCoding}
\alias{lazyFactorCoding}
\title{Efficient coding of factor levels}
\usage{
lazyFactorCoding(data, maxlevel = 10)
}
\arguments{
  \item{data}{Data frame in which to search for categorical
  variables.}

  \item{maxlevel}{Treat non-factor variables only if the
  number of unique values less than maxlevel. Defaults to
  10.}
}
\value{
R-code one line for each variable.
}
\description{
For all discrete and factor variables in a data set write
R-code which can quickly be modified to change the levels.
}
\details{
The code needs to be copy-and-pasted from the R-output
buffer into the R-code buffer. This can be customized for
the really efficiently working people e.g. in emacs.
}
\examples{
data(Diabetes)
lazyFactorCoding(Diabetes)
}
\author{
Thomas Alexander Gerds
}

