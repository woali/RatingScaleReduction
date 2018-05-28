\name{wineData}
\alias{wineData}
\docType{data}
\title{
Wine quality dataset
}

\description{
Two datasets are included, related to red and white vinho verde wine samples, from the north of Portugal.
}

\usage{data("wineData")}
\format{
  A data frame with 6497 observations on the following 13 variables.
  \describe{
    \item{\code{fixed.acidity}}{a numeric vector}
    \item{\code{volatile.acidity}}{a numeric vector}
    \item{\code{citric.acid}}{a numeric vector}
    \item{\code{residual.sugar}}{a numeric vector}
    \item{\code{chlorides}}{a numeric vector}
    \item{\code{free.sulfur.dioxide}}{a numeric vector}
    \item{\code{total.sulfur.dioxide}}{a numeric vector}
    \item{\code{density}}{a numeric vector}
    \item{\code{pH}}{a numeric vector}
    \item{\code{sulphates}}{a numeric vector}
    \item{\code{alcohol}}{a numeric vector}
    \item{\code{decision}}{a numeric vector}
  }
}


\references{
1. P. Cortez, A. Cerdeira, F. Almeida, T. Matos and J. Reis. Modeling wine preferences by data mining from physicochemical properties. In Decision Support Systems, Elsevier, 47(4):547-553, 2009. \cr
2. M. Lichman. UCI machine learning repository, 2013. \url{https://archive.ics.uci.edu/ml/datasets/wine+quality}\cr 
}
\examples{
data(wineData)
## maybe str(wineData) ; plot(wineData) ...
}
\keyword{datasets}
