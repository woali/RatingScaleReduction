\name{SHSData}
\alias{SHSData}
\docType{data}
\title{
Somerville Happiness Survey}
\description{
This survey has been given sent out to a random sample of
Somerville residents asking them to rate their personal happiness and their satisfaction with city
services every second year since 2011. Every year, the survey is refined. This data set corresponds to year 2015.
}
\usage{data("SHSData")}
\format{
  A data frame with 143 observations on the following 7 variables.
  \describe{
    \item{\code{X1}}{a numeric vector}
    \item{\code{X2}}{a numeric vector}
    \item{\code{X3}}{a numeric vector}
    \item{\code{X4}}{a numeric vector}
    \item{\code{X5}}{a numeric vector}
    \item{\code{X6}}{a numeric vector}
    \item{\code{D8}}{a numeric vector}
  }
}
\references{
Somerville-happinesssurvey-responses-2011-2013-2015}

\examples{
data(SHSData)
## maybe str(SHSData) ; plot(SHSData) ...
}
\keyword{datasets}
