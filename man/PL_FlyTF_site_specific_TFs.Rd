\name{PL_FlyTF_site_specific_TFs}
\alias{PL_FlyTF_site_specific_TFs}
\docType{data}
\title{
PL_FlyTF_site_specific_TFs data
}
\description{
A dataset containing identifiers of transcription factors with experimental evidence for both DNA-binding and transcriptional regulatory function. This data comes from \url{www.flyTF.org}, the drosophila transcription factor database version 2.
}
\usage{data("PL_FlyTF_site_specific_TFs")}
\format{
  A data frame with 171 observations on the following 5 variables:
  \describe{
    \item{Index}{Index of observations}
    \item{Gene.secondaryIdentifier}{InterMine Gene.secondaryIdentifier}
    \item{Gene.symbol}{Gene symbol}
    \item{Gene.primaryIdentifier}{InterMine Gene.primaryIdentifier}
    \item{Gene.organism.name}{Gene organism name}
  }
}

\source{
\url{http://www.flymine.org/flymine/bag.do?subtab=view}
}

\examples{

data(PL_FlyTF_site_specific_TFs)

}

\keyword{datasets}
