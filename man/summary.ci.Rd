\name{summary.ci}
\alias{summary.ci}
\title{Summarize confidence intervals}
\usage{
\method{summary}{ci}(object, digits = 3, format, se = FALSE, print = TRUE,
  ...)
}
\arguments{
  \item{object}{Object containing point estimates and the
  corresponding confidence intervals}

  \item{digits}{Digits for round values}

  \item{se}{If \code{TRUE} add standard error.}

  \item{print}{Logical: if \code{FALSE} do not actually
  print confidence intervals but just return them
  invisibly.}

  \item{...}{passed to print}

  \item{format}{A string which indicates the format used
  for confidence intervals. The string is passed to
  \code{\link{formatCI}} with two arguments: the lower and
  the upper limit. For example \code{'(l;u)'} yields
  confidence intervals with round parenthesis in which the
  upper and the lower limits are separated by semicolon.}
}
\value{
Formatted confidence intervals
}
\description{
Summarize confidence intervals
}
\details{
This format of the confidence intervals is
user-manipulable.
}
\examples{
library(lava)
m <- lvm(Y~X)
m <- categorical(m,Y~X,K=4)
set.seed(4)
d <- sim(m,24)
ci.mean(Y~X,data=d)
x <- summary(ci.mean(Y~X,data=d))
x
}
\author{
Thomas A. Gerds <tag@biostat.ku.dk>
}
\seealso{
ci plot.ci format.ci
}

