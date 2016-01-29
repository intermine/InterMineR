\name{getRegionFeature}
\alias{getRegionFeature}
\title{
Obtain the features (exons, transcripts, genes) in a region of the genome in bed format
}
\description{
Obtain the features (exons, transcripts, genes) in a region of the genome in bed format
}
\usage{
getRegionFeature(im, regions, featureType, organism = "H. sapiens", extension = 100, 
      isInterbase = F, timeout = 60)
}
\arguments{
  \item{im}{
a list containing the base URL and API token.
}
  \item{regions}{
a character vector, representing genomic regions, e.g., "X:99000000..99895088".
}
  \item{featureType}{
a character vector, represeting the type of features.  It could be any of these values: "Gene", "Transcript", "Exon".

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
a data.frame object, representing the information of the feature (exon, intron or gene) in bed format.
}
\author{
InterMine Team
}


\examples{
im <- initInterMine("humanmine.org/humanmine") #replace TOKEN with your token

gf <- getRegionFeature(im, c("3:12287368-12434356"), c("Gene"))
}
