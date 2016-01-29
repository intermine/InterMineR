\name{getRegionSequence}
\alias{getRegionSequence}
\title{
Obtain the DNA sequence in regions of the genome
}
\description{
Obtain the DNA sequence in regions of the genome
}
\usage{
getRegionSequence(im, regions, organism = "H. sapiens", extension = 100, 
      isInterbase = F, timeout = 60)
}
\arguments{
  \item{im}{
a list containing the base URL and API token.
}
  \item{regions}{
a character vector, representing genomic regions, e.g., "X:99000000..99895088".
}

  \item{organism}{
a string, representing the name of the organism.
}
  \item{extension}{
an integer, representing how far, in base-pairs, to extend the regions on each side.
}
  \item{isInterbase}{
a boolean, representing whether to treat the region as interbase co-ordinates.
}

  \item{timeout}{
an integer, representing the number of seconds to wait for the webservice to respond.
}
}
\value{
a XstringSet object, representing the DNA sequences in the specified regions.
}
\author{
InterMine Team
}


\examples{
im <- initInterMine("humanmine.org/humanmine")

#gf <- getRegionSequence(im, c("X:99000000..99895088"))
gf <- getRegionSequence(im, c("11:17200000..17500000"))

}
