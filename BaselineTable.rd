\name{BaselineTable}
\alias{BaselineTable}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
BaselineTable(formula, data = parent.frame(), type, summary.continuous = "mean(x) (sd(x))", stats = c("mean", "sd", "median", "iqr", "minmax"), sep = "-", digits = 2, ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
%%     ~~Describe \code{formula} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
}
  \item{type}{
%%     ~~Describe \code{type} here~~
}
  \item{summary.continuous}{
%%     ~~Describe \code{summary.continuous} here~~
}
  \item{stats}{
%%     ~~Describe \code{stats} here~~
}
  \item{sep}{
%%     ~~Describe \code{sep} here~~
}
  \item{digits}{
%%     ~~Describe \code{digits} here~~
}
  \item{\dots}{
%%     ~~Describe \code{\dots} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.


u <- BaselineTable(Sex~Age,data=malaria,summary.continuous="median(x) (iqr(x))",sep=";")
do.call("rbind",u)
do.call("cbind",u)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
