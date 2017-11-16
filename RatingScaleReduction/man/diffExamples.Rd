\name{diffExamples}
\alias{diffExamples}

\title{The number of different (unique) examples in a dataset}

\description{Datasets often contain replications. In particular,
one example may be replicated  n times, where n is the total
number of examples, so that there are no other examples.
Such situation would deviate computations and should be early detected.
Ideally, no example should be replicated but if the rate is small,
we can progress to computing AUC.}


\usage{
diffExamples(attribute)
}

\arguments{
  \item{attribute}{	a matrix or data.frame containing attributes}}


\value{
\item{total.examples}{a number of examples in a data}
\item{diff.examples}{a number of different examples in a data}
\item{dup.exapmles}{a number of duplicate examples in a data}
}


\author{Waldemar W. Koczkodaj, Feng Li,Alicja Wolny-Dominiak
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

#show the number of different examples
diffExamples(attribute)
}
