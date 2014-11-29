\name{formatCI}
\alias{formatCI}
\title{Formatting confidence intervals}
\usage{
formatCI(x, lower, upper, format = "[l;u]", degenerated = "asis",
  digits = 3, nsmall = digits, trim = TRUE, sep = "", ...)
}
\arguments{
  \item{x}{not used (for compatibility with format)}

  \item{lower}{Vector of lower limits}

  \item{upper}{Vector of upper limits}

  \item{format}{Character string in which 'l' will be
  replaced by the value of the lower limit (argument lower)
  and 'u' by the value of the upper upper limit. For
  example, \code{'(l,u)'} yields confidence intervals in
  round parenthesis in which the upper and lower limits are
  comma separated. Default is '[l;u]'.}

  \item{degenerated}{String to show when lower==upper.
  Default is '--'}

  \item{digits}{For rounding of values passed to format}

  \item{nsmall}{For rounding of values passed to format}

  \item{trim}{This argument is passed to format}

  \item{sep}{Field separator}

  \item{...}{not used}
}
\value{
String vector with confidence intervals
}
\description{
Format confidence intervals
}
\details{
The default format for confidence intervals is [lower;
upper].
}
\examples{
x=ci.mean(rnorm(10))
format(x)
formatCI(lower=c(0.001,-2.8413),upper=c(1,3.0008884))
# change format
formatCI(lower=c(0.001,-2.8413),upper=c(1,3.0008884),format="(l, u)")

# the first lower limit is shorter than the second, to align numbers
# use trim:
formatCI(lower=c(0.001,-2.8413),upper=c(1,3.0008884),format="l--u",trim=FALSE)
}
\author{
Thomas A. Gerds <tag@biostat.ku.dk>
}
\seealso{
plot.ci ci.mean
}

