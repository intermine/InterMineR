\name{getGeneIds}
\alias{getGeneIds}

\title{
Get Gene.id values for a list of gene identifiers
}
\description{
This function takes as input a list of gene identifiers and retrieves their unique Gene.id values for a specific organism. 
}
\usage{
getGeneIds(im, genes, organism)
}

\arguments{
  \item{im}{
a list containing the base URL and API token.
}
  \item{genes}{
a character vector containing the gene identifiers.
}
  \item{organism}{
a character string for the name of the Organism.
}
}

\details{
This function was designed to work in conjuction with the doEnrichment function. 
}
\value{
getGeneIds returns a list containing a data.frame of unique gene identifiers as well as the Gene.id values as a comma-separated string, ready to be used as input for the ids argument of the doEnrichment function.

Furthermore, in case of genes that return multiple identifiers or no identifiers, these are also reported separately in a data.frame and a character vector respectively.

  \item{unique.results }{data.frame with unique gene identifiers}
  \item{doEnrichment.string }{comma-separated character string of Gene.id values}
  \item{multiple.results }{data.frame with genes returning multiple identifiers}
  \item{genes.with.no.results }{character vector with genes returning no identifiers}
}
\author{
InterMine Team
}
\note{
doEnrichment.string can be passed as input in the doEnrichment() function.
}

\examples{

# HumanMine and Homo sapiens genes 

hsa.genes <- c("ABCC8", "ACE", "AKT2", "APPL1", "AQP2", "AVP", "AVPR2", "BLK", "CAPN10", "CCR5")

list.hsa <- getGeneIds(
  im = initInterMine(listMines()["HumanMine"]),
  organism = "Homo sapiens",
  genes = hsa.genes # must be a character vector!!!
)

# FlyMine and Drosophila melanogaster genes

fly.genes <- c("fkh", "vvl", "BEAF-32", "Antp", "Scr", "abd-A", "bcd", "Ubx", "zen", "ara")

list.fly <- getGeneIds(
  im = initInterMine(listMines()["HumanMine"]),
  organism = "Homo sapiens",
  genes = fly.genes # must be a character vector!!!
)

}
