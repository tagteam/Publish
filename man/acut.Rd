% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/acut.R
\name{acut}
\alias{acut}
\title{Automatic selection and formatting of breaks in \code{cut}}
\usage{
acut(
  x,
  n = 5,
  type = "default",
  format = NULL,
  format.low = NULL,
  format.high = NULL,
  dig.lab = 3,
  right = TRUE,
  breaks,
  labels = TRUE,
  ...
)
}
\arguments{
\item{x}{a numeric vector which is to be converted to a factor by cutting (passed directly to \code{cut}).}

\item{n}{number of bins to create based on the empirical quantiles of x. This will be overruled if \code{breaks} is supplied.}

\item{type}{a high-level formatting option. For now, the only other option than the default setting is "\code{age}". See details and examples.}

\item{format}{string used to make labels. \%l and \%u identifies the lower and upper value of the breaks respectively. See examples.}

\item{format.low}{string used specifically on the lowest label.}

\item{format.high}{string used specifically on the highest label.}

\item{dig.lab}{integer which is used when labels are not given. It determines the number of digits used in formatting the break numbers. (Passed directly to \code{cut}.)}

\item{right}{logical, indicating if the intervals should be closed on the right (and open on the left) or vice versa (passed directly to \code{cut}).}

\item{breaks}{specify breaks manually as in \code{cut}.}

\item{labels}{logical, indicating whether or not to make labels or simply use ordered numbers. If TRUE, the labels are constructed as discribed above.}

\item{...}{further arguments passed to \code{cut}.}
}
\value{
same as for cut. A vector of 'factors' is created, unless 'labels=FALSE'.
}
\description{
A version of \code{cut} that easily formats the labels and places breaks by default.
}
\details{
The formats are supplied by specifiyng the text around the lower (\%l) and upper (\%l) value (see examples).
If user specified breaks are supplied, the default labels from \code{cut} are used.
If automatic breaks are used, the default labels are a slight modification at the end point of the default from \code{cut}
All this can of course be adjusted manually through the format functionality (see below). 

By default, 5 breaks are constructed according to the quantiles with of the input \code{x}.
The number of breaks can be adjusted, and default specifying breaks (as in \code{cut}) can be supplied instead.

If \code{type} is changed from "\code{default}" to another option, a different formatting template is used.
For now the only other option is "\code{age}", which is designed to be well suited to easily group age variables.
When \code{type}="\code{age}" only the \code{breaks} argument is used, and it behaves different from otherwise.
If a single number is supplied, intervals of length \code{breaks} will automatically be constructed (starting from 0).
If a vector is supplied, the intervals are used as in \code{cut} but formatted differently, see examples.
}
\examples{
data(Diabetes) # load dataset

## The default uses format similar to cut
chol.groups <- acut(Diabetes$chol)
table(chol.groups)

## The formatting can easily be changed
chol.groups <- acut(Diabetes$chol,format="\%l-\%u",n=5)
table(chol.groups)

## The default is to automatic place the breaks, so the number of this can easily be changed.
chol.groups <- acut(Diabetes$chol,n=7)
table(chol.groups)

## Manually setting format and breaks
age.groups <- acut(Diabetes$age,format="\%l-\%u",breaks=seq(0,100,by=10))
table(age.groups)

## Other variations 
age.groups <- acut(Diabetes$age,
                   format="\%l-\%u",
                   format.low="below \%u",
                   format.high="above \%l",
                   breaks=c(0, seq(20,80,by=10), Inf))
table(age.groups)

BMI.groups <- acut(Diabetes$BMI,
                   format="BMI between \%l and \%u",
                   format.low="BMI below \%u",
                   format.high="BMI above \%l")
table(BMI.groups)
org(as.data.frame(table(BMI=BMI.groups)))

## Instead of using the quantiles, we can specify equally spaced breaks,
## but still get the same formatting
BMI.grouping <-
   seq(min(Diabetes$BMI,na.rm=TRUE), max(Diabetes$BMI,na.rm=TRUE), length.out=6)
BMI.grouping[1] <- -Inf # To get all included
BMI.groups <- acut(Diabetes$BMI,
                   breaks=BMI.grouping,
                   format="BMI between \%l and \%u",
                   format.low="BMI below \%u",
                   format.high="BMI above \%l")
table(BMI.groups)
org(as.data.frame(table(BMI=BMI.groups)))

## Using type="age"
## When using type="age", categories of 10 years are constructed by default.
## The are formatted to be easier to read when the values are ages.
table(acut(Diabetes$age, type="age"))

## This can be changes with the breaks argument.
## Note that this is diffent from cut when breaks is a single number.
table(acut(Diabetes$age, type="age", breaks=20))

## Of course We can also supply the breaks manually.
## The formatting depends on whether or not all the values fall within the breaks:
## All values within the breaks
table(acut(Diabetes$age, type="age", breaks=c(0, 30, 50, 80, 100)))
## Some values below and above the breaks
table(acut(Diabetes$age, type="age", breaks=c(30, 50, 80))) 

}
\author{
Anders Munch
}
