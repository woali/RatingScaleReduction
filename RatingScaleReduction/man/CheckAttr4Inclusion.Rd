\name{CheckAttr4Inclusion}

\alias{CheckAttr4Inclusion}

\title{Check the next attribute for possible inclusion into AUC}
\description{The attribute checked for AUC before it is added to the running total.
The running total is used with the class (decision attribute)
to compute AUC. The next attribute is added to the sequence of attributes having the MAX total AUC.}


\usage{
CheckAttr4Inclusion(attribute, D, plotCheck=FALSE, method=c("delong", "bootstrap",
"venkatraman", "sensitivity", "specificity"), boot.n,
alternative = c("two.sided", "less", "greater"))
}

\arguments{
  \item{attribute}{	a matrix or data.frame containing attributes}
  \item{D}{ the decision vector }
  \item{plotCheck}{If TRUE the plot with two ROC curves is created}
  \item{method}{the method to useas in the function roc.test\{pROC\}}
  \item{boot.n}{boostrap replication number}
  \item{alternative}{the alternative hipothesis}
}


\value{
\item{test}{the result of the roc.test as in the function roc.test from the package pROC}}


\references{
1. R. DeLong, D. M. DeLong, and D. L. Clarke-Pearson. Comparing the areas under two or more
correlated receiver operating characteristic curves: a nonparametric approach. Biometrics, pages
837 - 845, 1988.\cr 

2. W.W. Koczkodaj,  T. Kakiashvili,  A. Szymanska, 
 J. Montero-Marin, R. Araya, J. Garcia-Campayo, K. Rutkowski,
 D. Strzalka,
 How to reduce the number of rating scale items without
 predictability loss? Scientometrics,909(2):581-593(open access), 2017 \cr
 \url{https://link.springer.com/article/10.1007/s11192-017-2283-4} \cr}
 
 
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
decision <-as.numeric(outcome)

#deLong test, two-side alternative hiphotesis
CheckAttr4Inclusion(attribute, decision, method=c("delong"), 
alternative=c("two.side"))

#bootstrap, two-side alternative hiphotesis
#CheckAttr4Inclusion(attribute, decision, method=c("bootstrap"), boot.n=500)
#}
