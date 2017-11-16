\name{startAuc}
\alias{startAuc}

\title{AUC of a single attribute}

\description{
Compute AUC of every single attribute}

\usage{
startAuc(attribute, D)}

\arguments{
  \item{attribute}{	a matrix or data.frame containing attributes}
  \item{D}{ the decision vector }
}

\value{
\item{auc}{AUC of a single attribute}
\item{item}{attribute labels}
\item{summary}{a summary table}
}


\references{
1. W.W. Koczkodaj,  T. Kakiashvili,  A. Szymanska, J. Montero-Marin, R. Araya, J. Garcia-Campayo, K. Rutkowski, D. Strzalka,
 How to reduce the number of rating scale items without
 predictability loss? Scientometrics, 909(2):581-593(open access), 2017 \cr
 \url{https://link.springer.com/article/10.1007/s11192-017-2283-4} \cr

2. X. Robin, N. Turck, A. Hainard, N. Tiberti, F. Lisacek, J.-C. Sanchez, and M. Muller. proc: an opensource
package for r and s+ to analyze and compare roc curves. BMC Bioinformatics, 2011 \cr
\url{https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-12-77} \cr
}
 
\author{Waldemar W. Koczkodaj, Alicja Wolny-Dominiak
}

\examples{
#creating the matrix of attributes and the decision vector
#must be as.numeric()
data(aSAH)
attach(aSAH)
is.numeric(aSAH)

attribute <-data.frame(as.numeric(gender), 
as.numeric(age), as.numeric(wfns), as.numeric(s100b), as.numeric(ndka))
colnames(attribute) <-c("a1", "a2", "a3", "a4", "a5")
decision <-as.numeric(outcome)

#compute AUC of all attributes
start <-startAuc(attribute, decision)
start$summary
}
