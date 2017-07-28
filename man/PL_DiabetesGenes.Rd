\name{PL_DiabetesGenes}
\alias{PL_DiabetesGenes}
\docType{data}
\title{
PL_DiabetesGenes data
}

\description{
A dataset containing identifiers of genes associated with all forms of Diabetes according to OMIM \url{http://www.omim.org/}.
}

\usage{data("PL_DiabetesGenes")}

\format{
  A data frame with 68 observations on the following 6 variables:
  
  \describe{
    \item{Gene.symbol}{Gene symbol}
    \item{Gene.name}{Gene whole name}
    \item{Gene.primaryIdentifier}{InterMine Gene.primaryIdentifier (ENTREZ identifier)}
    \item{Gene.secondaryIdentifier}{InterMine Gene.secondaryIdentifier (ENSEMBLE identifier)}
    \item{Gene.length}{Gene length in base pairs}
    \item{Gene.organism.name}{Gene organism name}
  }
  
}

\source{
\url{http://www.humanmine.org/humanmine/bag.do?subtab=view}
}

\examples{

data(PL_DiabetesGenes)

}

\keyword{datasets}
