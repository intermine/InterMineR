\name{doEnrichment}
\alias{doEnrichment}
\title{
Do enrichment analysis for a list of genes
}
\description{
Do enrichment analysis for a list of genes.
}
\usage{
doEnrichment(im, genelist, ontology, subcategory = "", maxp = 0.05, 
correction = c("Holm-Bonferroni", "Benjamini and Hochberg", "Bonferroni", "None"), 
timeout = 60)
}
\arguments{
  \item{im}{
a list, containing the base URL and API token.
}
  \item{genelist}{
a character vector, represetning the genes for enrichment analysis
}
  \item{ontology}{
a string, representing the name of the ontology.  It could be of one of the following values:
"go_enrichment_for_gene" (for GO enrichment), "pathway_enrichment" (for pathway enrichment), 
"prot_dom_enrichment_for_gene" (for Protein domain enrichment), "publication_enrichment" (for Publication enrichment).
}
  \item{subcategory}{
a string, representing the sub-category of the ontology.  It could be of the following values:
"biological_process", "cellular_component", "molecular_function" (for GO enrichment) or "All", "KEGG pathways data set", "Reactome data set" (for pathway enrichment)
}
  \item{maxp}{
a numeric, representing the threshold p-value.
}
  \item{correction}{
a string, representing the correction method for multiple comparison.  It could be of one of the following values: "Holm-Bonferroni", "Benjamini and Hochberg", "Bonferroni", "None".
}

  \item{timeout}{
an integer, representing the number of seconds to wait for the webservice to respond.
}
}
\value{
a matrix, representing the enrichment result.
}
\author{
InterMine Team
}
\examples{
im <- initInterMine("humanmine.org/humanmine", "TOKEN") #replace TOKEN with your token

\dontrun{doEnrichment(im, c("ABO", "ALB"), 'go_enrichment_for_gene', 'cellular_component',
      correction="NONE")}
}