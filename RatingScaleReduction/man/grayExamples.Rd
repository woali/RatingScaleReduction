\name{grayExamples}
\alias{grayExamples}

\title{Examples belonging to both classes}

\description{A subset of data with examples having identical values
on all attributes (excluding the class attribute also called
the decision attribute which is different and has two permited values:
positive and negative)}

\usage{
grayExamples(attribute, D)
}

\arguments{
  \item{attribute}{	a matrix or data.frame containing attributes}
  \item{D}{ the decision vector }
}

\value{
\item{1}{a list of pairs of identical examples on all atributes}}

\author{Waldemar W. Koczkodaj, Alicja Wolny-Dominiak}

\examples{
#generate data

a=c(); attribute=c()
for (i in 1:3){
a <-sample(c(1,2,3), 100, replace=TRUE)
attribute <-cbind(attribute, a)
attribute=data.frame(attribute)
}
colnames(attribute)=c("a1", "a2", "a3")
names(attribute)

decision=sample(c(0,1), 100, replace=TRUE)

#check examples
grayExamples(attribute, decision)
}
