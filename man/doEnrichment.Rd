\name{doEnrichment}
\alias{doEnrichment}

\title{
Perform enrichment analysis
}
\description{
Retrieve enrichment analysis results from InterMine platform. Enrichment widgets provide a statistical summary of what makes a list distinct from the background population over a certain domain. They return a list of members of the domain ranked by p-value (low to high).
}
\usage{
doEnrichment(
  im, 
  genelist = NULL,
  ids = NULL,
  widget = NULL,
  population = NULL, 
  maxp = 0.05,
  correction = "Benjamini Hochberg", 
  filter = NULL,
  output = "xml"
)
}

\arguments{
  \item{im}{
a list containing the base URL and API token.
}
  \item{genelist}{
The name of the list to investigate, optional unless ids (identifiers) is NULL.
}
  \item{ids}{
Comma-separated character string of InterMine object IDs, optional unless list is NULL. Use getGeneIds() function to retrieve unique Gene.id values for a list of gene identifiers.
}
  \item{widget}{
The name of the enrichment widget to display. Use getWidgets() function to retrieve available enrichment type widgets for the respective Mine.
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
An optional filter that some widgets accept. Use getWidgets() function to retrieve available filters of the respective enrichment widget.
}
  \item{output}{
output format which will be be processed to data.frame (Use "json" option only after installing InterMineR and jsonlite independently!).
}
}
\details{
The public gene lists for each Mine are available at the websites obtained by the following command:
paste0(listMines()$URL, "/bag.do?subtab=view")

Each type of enrichment widget can be applied to a gene list with specific annotation. To apply the enrichment analysis to the appropriate list of genes, use the targets column of the respective enrichment widgets as they are retrieved by getWidgets() function.
}
\value{

doEnrichment function returns a list containing the following values:

\item{data}{A data.frame containing the results of the enrichment analysis performed in InterMine platform. The statistically significant results are ordered by increasing p-values}

\item{populationCount}{a numeric value indicating the size of the reference population}

\item{notAnalyzed}{a numeric value indicatng the number of input features that were not included in the enrichment analysis}

\item{maxp}{the numeric value of maximum p-value of results to display}

\item{correction}{the error correction algorithm used for the enrichment analysis}

\item{im}{the list containing the base URL and API token}

\item{widget}{the name of the enrichment widget indicating the type of the analysis which was performed}

}

\author{
InterMine Team
}

\examples{

# FlyMine
enrichResults.FlyMine <- doEnrichment(
  im = initInterMine(mine = listMines()["FlyMine"]),
  genelist = 'PL FlyTF_site_specific_TFs',
  widget = "go_enrichment_for_gene"
)

# HumanMine
enrichResults.HumanMine <- doEnrichment(
  im = initInterMine(mine = listMines()["HumanMine"]),
  genelist = "PL_DiabetesGenes",
  widget = "go_enrichment_for_gene"
)

}
