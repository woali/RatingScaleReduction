\name{rsr}
\alias{rsr}

\title{Rating scale reduction}

\description{
This package implements a rather sophisticated method published in (Koczkodaj et al., 2017)
In essence, it is a stepwise method fro maximizing the area under the area (AUC) of receiver operating characteristic (ROC).
In this description, data mining terminology will be used:

\itemize{
\item {examples (observations in statistics),}
\item {variables in statistics,}
\item {class or decision attribute (decision variable may be used statistics).}
}
The implemented algorithm (when reduced to its minimum) comes to using a loop for all attributes (with the class excluded) to compute AUC. Subsequently, attributes are sorted in the descending order by AUC. The attribute with the largest AUC is added to a subset of all attributes (evidently, it cannot be  empty since it is supposed to be the minimum subset S of all attributes with the maximum AUC). We keep adding the next in line (according to AUC) attribute to the subset S checking AUC. If it decreases, we stop the procedure. The above procedure can be described by the following algorithm.

Algorithm:

\enumerate{

\item compute AUC of all attributes excluding class

\item sort attributes by their AUC in the ascending order

\item select the attribute with the largest AUC to subset S

\item select the next attribute A with the largest AUC to subset S

\item if the AUC of the subset S is larger that AUC of the former AUC then go to 3

}

There are a lot of checking (e.g., if the dataset is not empty or full of replications) involved.}

\usage{
rsr(attribute, D, plotRSR = FALSE, method=c('Stop1Max', 'StopGlobalMax'))
}
\arguments{
  \item{attribute}{a matrix or data.frame containing attributes}
  \item{D}{ the decision vector }
  \item{plotRSR}{If TRUE the ROC curve is ploted}
  \item{method}{the Stop reduction criteria: First Max of AUC or Global Max of AUC, default: 'Stop1Max'}
}


\value{
\item{rsr.auc}{total AUC of atrtibutes}
\item{rsr.label}{attribute labels }
\item{summary}{a summary table}
}
\references{
1. W.W. Koczkodaj,  T. Kakiashvili,  A. Szymanska, J. Montero-Marin, R. Araya, J. Garcia-Campayo, K. Rutkowski, D. Strzalka,
 How to reduce the number of rating scale items without
 predictability loss? Scientometrics, 909(2):581-593(open access), 2017 \cr
 \url{https://link.springer.com/article/10.1007/s11192-017-2283-4} \cr


2. T. Kakiashvili, W. W. Koczkodaj, and M. Woodbury-Smith. Improving the medical scale predictability
by the pairwise comparisons method: Evidence from a clinical data study. Computer Methods and
Programs in Biomedicine, 105(3), 2012 \cr
DOI: \url{http://dx.doi.org/10.1016/j.cmpb.2011.09.011} \cr

3. X. Robin, N. Turck, A. Hainard, N. Tiberti, F. Lisacek, J.-C. Sanchez, and M. Muller. proc: an opensource
package for r and s+ to analyze and compare roc curves. BMC Bioinformatics, 2011 \cr
\url{https://bmcbioinformatics.biomedcentral.com/articles/10.1186/1471-2105-12-77} \cr}


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

#rating scale reduction procedure
rsred <-rsr(attribute, decision, plotRSR=TRUE)
rsred
}
