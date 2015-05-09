\name{formatCI}
\alias{formatCI}
\title{Formatting confidence intervals}
\usage{
formatCI(x, lower, upper, handler = "sprintf", format = "[l;u]",
  degenerated = "asis", digits = 2, nsmall = digits, trim = TRUE,
  sep = "", ...)
}
\arguments{
  \item{x}{not used (for compatibility with format)}

  \item{lower}{Numeric vector of lower limits}

  \item{upper}{Numeric vector of upper limits}

  \item{handler}{Function to format numeric values. Default
  is 'sprintf', also supported are 'format' and
  'prettyNum'}

  \item{format}{Character string in which 'l' will be
  replaced by the value of the lower limit (argument lower)
  and 'u' by the value of the upper upper limit. For
  example, \code{'(l,u)'} yields confidence intervals in
  round parenthesis in which the upper and lower limits are
  comma separated. Default is '[l;u]'.}

  \item{degenerated}{String to show when lower==upper.
  Default is '--'}

  \item{digits}{If handler \code{format} or
  \code{prettyNum} used format numeric vectors.}

  \item{nsmall}{If handler \code{format} or
  \code{prettyNum} used format numeric vectors.}

  \item{trim}{Used to aligning resulting intervals. Also,
  if handler \code{format} or \code{prettyNum} used format
  numeric vectors .}

  \item{sep}{Field separator}

  \item{...}{passed to handler}
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
formatCI(lower=x[3],upper=x[4])
formatCI(lower=c(0.001,-2.8413),upper=c(1,3.0008884))
# change format
formatCI(lower=c(0.001,-2.8413),upper=c(1,3.0008884),format="(l, u)")

# if the first lower limit is shorter than the second (missing negative sign),
# then, option trim will make a difference:
formatCI(lower=c(0.001,-2.8413),upper=c(1,3.0008884),format="l--u",trim=FALSE)
formatCI(lower=c(0.001,-2.8413),upper=c(1,3.0008884),format="l--u",trim=TRUE)

# change of handler function
l <- c(-0.0890139,0.0084736,144.898333,0.000000001)
u <- c(0.03911392,0.3784706,3338944.8821221,0.00001)
cbind(format=formatCI(lower=l,upper=u,format="[l;u)",digits=2,nsmall=2,handler="format"),
      prettyNum=formatCI(lower=l,upper=u,format="[l;u)",digits=2,nsmall=2,handler="prettyNum"),
      sprintf=formatCI(lower=l,upper=u,format="[l;u)",digits=2,nsmall=2,handler="sprintf"))
}
\author{
Thomas A. Gerds <tag@biostat.ku.dk>
}
\seealso{
plot.ci ci.mean
}

