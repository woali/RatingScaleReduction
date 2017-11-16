\name{totalAuc}
\alias{totalAuc}

\title{AUC of the running total of attributes}
\description{
AUC values are computed for all individual attributes. We sort them in an ascending order. We
beging with the attribute having the largest AUC and add to it the second, third,... attribute until
AUC of the total of them decreases.}

\usage{
totalAuc(attribute, D, plotT = FALSE)}

\arguments{
  \item{attribute}{	a matrix or data.frame containing attributes}
  \item{D}{ the decision vector }
  \item{plotT}{If TRUE the plot is created: x - labels of atrributes, y - total AUC in ascending order}
}


\value{
\item{ordered.attribute}{ordered attribute matrix}
\item{total.auc}{total AUC}
\item{item}{ordered attribute labels }
\item{summary}{a summary table}
}


\references{
1. W.W. Koczkodaj,  T. Kakiashvili,  A. Szymanska, J. Montero-Marin, R. Araya, J. Garcia-Campayo, K. Rutkowski, D. Strzalka,
 How to reduce the number of rating scale items without
 predictability loss? Scientometrics, 909(2):581-593(open access), 2017\cr
 \url{https://link.springer.com/article/10.1007/s11192-017-2283-4} \cr


2. T. Kakiashvili, W. W. Koczkodaj, and M. Woodbury-Smith. Improving the medical scale predictability
by the pairwise comparisons method: Evidence from a clinical data study. Computer Methods and
Programs in Biomedicine, 105(3), 2012 \cr
DOI: \url{http://dx.doi.org/10.1016/j.cmpb.2011.09.011} \cr


3. X. Robin, N. Turck, A. Hainard, N. Tiberti, F. Lisacek, J.-C. Sanchez, and M. Muller. proc: an opensource
package for r and s+ to analyze and compare roc curves. BMC Bioinformatics, 2011 \cr
\url{https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-12-77}
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

#arrange start AUC in an ascending order and compute total AUC according to 
#Rating Scale Reduction procedure

tot <-totalAuc(attribute, decision, plotT=TRUE)
tot$summary
}
