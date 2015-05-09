\name{plot.regressionTable}
\alias{plot.regressionTable}
\title{Plotting regression coefficients with confidence limits}
\usage{
\method{plot}{regressionTable}(x, ...)
}
\arguments{
  \item{x}{regression table obtained with regressionTable}

  \item{...}{passed to plot.ci}
}
\description{
Plotting regression coefficients with confidence limits
}
\examples{
data(Diabetes)
f <- glm(bp.2s~bp.1s+age+chol+gender+location,data=Diabetes)
rtf <- regressionTable(f,factor.reference = "inline")
plot(rtf)

data(trace)
fit<-glm(dead ~ smoking+ sex+ age+Time+offset(log(ObsTime)), family = poisson,data=trace)
fit2<- regressionTable(fit,factor.reference = "inline")
plot(fit2)
}
\author{
Thomas A. Gerds <tag@biostat.ku.dk>
}
\seealso{
regressionTable
}

