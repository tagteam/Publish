\name{plot.ci}
\alias{plot.ci}
\title{Plot confidence intervals}
\usage{
\method{plot}{ci}(x, y.offset = 0, lower, upper, labels = TRUE, label.pos,
  title.labels, values = TRUE, value.pos = TRUE, title.values,
  factor.reference.pos, factor.reference.label = "Reference", pch = 16,
  cex = 1, lwd = 2, col = 1, xlim, ylim, ylab, xlab, automar = TRUE,
  margin.offset = 0.25, digits = 1, format = NULL, shift = 0,
  add = FALSE, axes = TRUE, axis2 = FALSE, ...)
}
\arguments{
  \item{x}{Object containing point estimates and the
  corresponding confidence intervals}

  \item{y.offset}{Adjustment of position of confidence
  intervals and text on y-axis. When \code{y.offset=0} the
  positions are integers between the number of intervals
  (upper most) and 1 (lowest interval).}

  \item{lower}{lower confidence limits}

  \item{upper}{upper confidence limits}

  \item{labels}{labels}

  \item{label.pos}{x-position of labels}

  \item{title.labels}{title for label column}

  \item{values}{Logical: if \code{TRUE} show the values of
  the point estimates and confidence intervals on the
  graph}

  \item{value.pos}{x-postion of values}

  \item{title.values}{Label for the column (or row) of the
  values.}

  \item{factor.reference.pos}{Position of factor
  references.}

  \item{factor.reference.label}{Label to use at
  factor.reference.pos instead of value.}

  \item{pch}{Point type for point estimates}

  \item{cex}{Size of points}

  \item{lwd}{Line type for the confidence intervals}

  \item{col}{Color for confidence intervals and point
  estimates. Can be controlled separately via points.col
  and segments.col}

  \item{xlim}{Limit of the x-axis}

  \item{ylim}{Limit of the y-axis}

  \item{ylab}{Label for the x-axis}

  \item{xlab}{Label for the y-axis}

  \item{automar}{If \code{TRUE} set margin elements 2 and 4
  of \code{par()$mar} based on string width of labels and
  values.}

  \item{margin.offset}{If \code{automar} is \code{TRUE}
  additional offset in inches added to margins.  Can be a
  vector of length 2, first element for left margin second
  for right margin.}

  \item{digits}{For rounding of values, passed to format
  and print.ci}

  \item{format}{A string which indicates the format used
  for confidence intervals.  The string is passed to
  \code{\link{formatCI}} with two arguments: the lower and
  the upper limit. For example \code{'(l;u)'} yields
  confidence intervals with round parenthesis in which the
  upper and the lower limits are separated by semicolon.}

  \item{shift}{Shift the whole graph \code{shift} units to
  the right if \code{shift} is positive and to the left if
  shift is negative. Useful for combining multiple plots.}

  \item{add}{If not \code{FALSE} add to an existing plot.}

  \item{axes}{if FALSE do not add axes}

  \item{axis2}{if FALSE do not y-axis}

  \item{...}{Used to transport arguments for the following
  subroutines: \code{"plot"}, \code{"points"},
  \code{"segments"}, \code{"labels"}, \code{"values"},
  \code{"title.labels"}, \code{"title.values"},
  \code{"axis1"}, \code{"axis2"}, \code{"background"}.}
}
\description{
Function to plot confidence intervals
}
\details{
Function to plot means and other point estimates with
confidence intervals
}
\examples{
data(Diabetes)
x=ci.mean(bp.2s~frame,data=Diabetes)
plot(x)
}
\author{
Thomas A. Gerds <tag@biostat.ku.dk>
}

