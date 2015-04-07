\name{labelUnits}
\alias{labelUnits}
\title{labelUnits}
\usage{
labelUnits(x, ...)
}
\arguments{
  \item{x}{A matrix obtained with \code{univariateTable}.}

  \item{...}{not used}
}
\value{
The re-labeled matrix
}
\description{
Label output tables
}
\details{
Change printed variable labels and values
}
\examples{
data(Diabetes)
tab <- summary(univariateTable(gender~AgeGroups+chol+waist,data=Diabetes))
publish(tab)
ltab <- labelUnits(tab,"chol"="Cholesterol (mg/dL)","age<40"="younger than 40")
publish(ltab)
}
\author{
Thomas A. Gerds <tag@biostat.ku.dk>
}
\seealso{
univariateTable
}

