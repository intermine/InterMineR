\name{doEnrichment}
\alias{doEnrichment}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Do enrichment analysis for a list of genes
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Do enrichment analysis for a list of genes.
}
\usage{
doEnrichment(im, genelist, ontology, subcategory = "", maxp = 0.05, 
correction = c("Holm-Bonferroni", "Benjamini and Hochberg", "Bonferroni", "None"), 
timeout = 60)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{im}{
%%     ~~Describe \code{im} here~~
a list, containing the base URL and API token.
}
  \item{genelist}{
%%     ~~Describe \code{genelist} here~~
a character vector, represetning the genes for enrichment analysis
}
  \item{ontology}{
%%     ~~Describe \code{ontology} here~~
a string, representing the name of the ontology.  It could be of one of the following values:
"go_enrichment_for_gene" (for GO enrichment), "pathway_enrichment" (for pathway enrichment), 
"prot_dom_enrichment_for_gene" (for Protein domain enrichment), "publication_enrichment" (for Publication enrichment).
}
  \item{subcategory}{
%%     ~~Describe \code{subcategory} here~~
a string, representing the sub-category of the ontology.  It could be of the following values:
"biological_process", "cellular_component", "molecular_function" (for GO enrichment) or "All", "KEGG pathways data set", "Reactome data set" (for pathway enrichment)
}
  \item{maxp}{
%%     ~~Describe \code{maxp} here~~
a numeric, representing the threshold p-value.
}
  \item{correction}{
%%     ~~Describe \code{correction} here~~
a string, representing the correction method for multiple comparison.  It could be of one of the following values: "Holm-Bonferroni", "Benjamini and Hochberg", "Bonferroni", "None".
}

  \item{timeout}{
%%     ~~Describe \code{timeout} here~~
an integer, representing the number of seconds to wait for the webservice to respond.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
a matrix, representing the enrichment result.
}
%\references{
%% ~put references to the literature/web site here ~
%}
\author{
%%  ~~who you are~~
InterMine Team
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
im <- initInterMine("metabolicmine.org/beta", "TOKEN") #replace TOKEN with your token

\dontrun{doEnrichment(im, c("ABO", "ALB"), 'go_enrichment_for_gene', 'cellular_component',
      correction="NONE")}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
