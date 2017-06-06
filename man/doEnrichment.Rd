\name{doEnrichment}
\alias{doEnrichment}

\title{
Perform enrichment analysis
}
\description{
Retrieve enrichment analysis results from InterMine platform. Enrichment widgets provide a statistical summary of what makes a list distinct from the background population over a certain domain. They return a list of members of the domain ranked by p-value (low to high).
}
\usage{
doEnrichment = function(
  im, 
  genelist = NULL,
  ids = NULL,
  widget = NULL,
  population = NULL, 
  maxp = 0.05,
  correction = "Holm-Bonferroni", 
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
Comma-separated list of InterMine object IDs, optional unless list is NULL.
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
The error correction algorithm to use. Alternatively use "Benjamini Hochberg", "Bonferroni" or "None".
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
}
\value{

A data.frame containing the results of the enrichment analysis performed in InterMine platform.

The statistically significant results are ordered by increasing p-values.

}

\author{
InterMine Team
}

\examples{
enrichResults = doEnrichment(
  im = initInterMine(mine = listMines()["FlyMine"]),
  genelist = "PL FlyTF_site_specific_TFs",
  widget = "go_enrichment_for_gene"
  )
}
