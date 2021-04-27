\name{doEnrichment}
\docType{methods}

\alias{webservice-methods}
\alias{doEnrichment}
\alias{doEnrichment-methods}
\alias{doEnrichment,webservice-methods}

\title{
Perform enrichment analysis
}
\description{
Retrieve enrichment analysis results from InterMine platform. Enrichment widgets provide a statistical summary of what makes a list distinct from the background population over a certain domain. They return a list of members of the domain ranked by p-value (low to high).
}
\usage{
doEnrichment(
  Service-class, 
  genelist = NULL,
  ids = NULL,
  widget = NULL,
  population = NULL, 
  maxp = 0.05,
  correction = "Benjamini Hochberg", 
  filter = NULL,
  organism = NULL
)
}

\arguments{
  \item{Service-class}{
a \code{Service-class} object containing the base URL and API token.
}
  \item{genelist}{
The name of the list to investigate, optional unless ids (identifiers) is NULL.
}
  \item{ids}{
  
  a character vector containing the identifiers (list of genes, proteins, SNPs, etc.) for the enrichment analysis, optional unless genelist is NULL.
  
  Alternatively, a comma-separated character string of InterMine object IDs can be assigned directly to this argument. Use \code{\link{getGeneIds}} function to retrieve unique Gene.id values for a list of gene identifiers.

}
  \item{widget}{
The name of the enrichment widget to display. Use \code{\link{getWidgets}} function to retrieve available enrichment type widgets for the respective Mine.
}
  \item{population}{
The name of the list to use as the background population.
}
  \item{maxp}{
The maximum p-value of results to display. The range is 0.0 - 1.0
}
  \item{correction}{
The error correction algorithm to use. Possible options are "Benjamini Hochberg" (default), "Holm-Bonferroni", "Bonferroni" or "None".
}
  \item{filter}{
An optional filter that some widgets accept. Use \code{\link{getWidgets}} function to retrieve available filters of the respective enrichment widget.
}
  \item{organism}{
a character string defining the name of the organism (e.g. "Homo sapiens"). 

This argument is optional and can be used to limit the analysis to the list of identifiers for a specific organism. Can be useful for InterMine instances that contain data from multiple organisms.
}
}
\details{
The public gene lists for each Mine are available at the websites obtained by the following command:
paste0(listMines()["Mine_Name"], "/bag.do?subtab=view")

Each type of enrichment widget can be applied to a gene list with specific annotation. To apply the enrichment analysis to the appropriate list of genes, use the targets column of the respective enrichment widgets as they are retrieved by \code{\link{getWidgets}} function.
}
\value{
doEnrichment function returns a list containing the following values:

\item{data}{A data.frame containing the results of the enrichment analysis performed in InterMine platform. The statistically significant results are ordered by increasing p-values}

\item{populationCount}{a numeric value indicating the size of the reference population}

\item{notAnalyzed}{a numeric value indicatng the number of input features that were not included in the enrichment analysis}

\item{im}{the list containing the base URL and API token}

\item{parameters}{a character vector containing all the parameters used for the enrichment analysis, except im}

}

\author{
InterMine Team
}

\seealso{
\code{\link{getWidgets}}, \code{\link{getGeneIds}}
}

\examples{
# FlyMine
# The example has been hashed out as it requires an API token to run
# im.fly = initInterMine(listMines()["HumanMine"],"Your token") #change here to your token
# class(im.fly)
# enrichResults.FlyMine <- doEnrichment(
# im.fly,
# genelist = "PL FlyAtlas_brain_top",
#  #genelist = 'PL FlyTF_site_specific_TFs',
#  widget = "go_enrichment_for_gene"
#)

# use ids instead of genelist
#data("PL_FlyTF_site_specific_TFs")
#enrichResults.FlyMine <- doEnrichment(
#  im.fly,
  #genelist = 'PL FlyTF_site_specific_TFs',
#  ids = PL_FlyTF_site_specific_TFs$Gene.primaryIdentifier,
#  widget = "go_enrichment_for_gene"
#)

# HumanMine
#im.human = initInterMine(listMines()["HumanMine"],"Your token") #change here to your token
#class(im.human)
#introduce the name of a genelist that you have created or not
#enrichResults.HumanMine <- doEnrichment(
#  im.human,
#  genelist = "copy_of_intersected",
#  widget = "go_enrichment_for_gene"
#  ) 

# use ids instead of genelist
#data("PL_DiabetesGenes")
#enrichResults.HumanMine <- doEnrichment(
#  im.human,
  #genelist = "PL_DiabetesGenes",
#  ids = PL_DiabetesGenes$Gene.primaryIdentifier,
#  widget = "go_enrichment_for_gene"
#)
}
