\name{convertToGeneAnswers}
\alias{convertToGeneAnswers}

\title{
Convert InterMineR enrichment analysis results to GeneAnswers object
}
\description{
\code{convertToGeneAnswers} constitutes a wrapper function for converting the results of \code{\link{doEnrichment}} function to a \code{\link[GeneAnswers]{GeneAnswers-class}} object. This way the user can utilize the functions of \code{\link[GeneAnswers]{GeneAnswers}} package to visualize the results of InterMineR enrichment analysis.
}
\usage{
convertToGeneAnswers(
  enrichmentResult,
  geneInput,
  geneInputType,
  geneExprProfile,
  annLib,
  categoryType,
  enrichCategoryChildName
)
}

\arguments{
  \item{enrichmentResult}{
  a list containing the results of \code{\link{doEnrichment}} function.}
\item{geneInput}{
a data.frame containing the gene identifiers used for the InterMineR enrichment analysis, and possible values associated with them.
}
\item{geneInputType}{
a character string specifying the InterMineR gene identifier type of the values assigned to geneInput argument. InterMineR identifiers for each Mine can be retrieved with \code{\link{getModel}} function.
}
\item{geneExprProfile}{
a data.frame containing gene expression information (optional)
}
\item{annLib}{
name of given annotation library file or user provided annotation list.
}
\item{categoryType}{
name of given annotation category or NULL for user provided annotation list.
}
\item{enrichCategoryChildName}{
a character string specifying the InterMIneR annotation category identifier. This argument must be assigned manually if:

1) 'enrichIdentifier' column is missing from the widgets of the Mine used by \code{\link{doEnrichment}},

2) the value of 'enrichIdentifier' for the enrichment widget used by \code{\link{doEnrichment}} is NA.

InterMineR identifiers for each Mine can be retrieved with \code{\link{getModel}} function.
}
}

\value{
a \code{\link[GeneAnswers]{GeneAnswers-class}} object.
}
\references{
\url{https://bioconductor.org/packages/release/bioc/html/GeneAnswers.html}
}
\author{
InterMine Team
}

\seealso{
\code{\link{doEnrichment}}, \code{\link[GeneAnswers]{GeneAnswers-class}}, \code{\link{getWidgets}}, \code{\link{getModel}}
}
\examples{
# load human genes which are associated with Diabetes (retrieved from HumanMine)
data("PL_DiabetesGenes")

# get Gene.primaryIdentifiers (ENTREZ Gene identifier)
hsa_gene_entrez = as.character(PL_DiabetesGenes$Gene.primaryIdentifier)

# perform enrichment analysis with InterMineR
im.human = initInterMine(listMines()["HumanMine"],"Your token") #change here to your token
class(im.human)
hsa_enrichResult = doEnrichment(
  im.human,
  genelist = "PL_DiabetesGenes",
  widget = "go_enrichment_for_gene",
  correction = "Benjamini Hochberg"
)

# convert InterMineR enrichment analysis results to a GeneAnswers object
# Do not run unless you have installed and loaded GeneAnswers package!

# load GeneAnswers
library(GeneAnswers)
  
hsa_geneanswers = convertToGeneAnswers(
  enrichmentResult = hsa_enrichResult,
  geneInput = data.frame(GeneID = as.character(hsa_gene_entrez), 
                         stringsAsFactors = FALSE),
  geneInputType = "Gene.primaryIdentifier",
  annLib = 'org.Hs.eg.db',
  categoryType = "GO"
  # enrichCategoryChildName = "Gene.goAnnotation.ontologyTerm.parents.identifier"
)

}
