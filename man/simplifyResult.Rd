\name{simplifyResult}
\alias{simplifyResult}

\title{
Convert multiple values of a column into Comma-separated character strings
}
\description{
This function converts the values of a column variable within a dataset into comma-separated, character strings. The process is achieved by using another column of the same dataset as index.
}
\usage{
simplifyResult(
  dataset,
  index_column,
  values_column,
  returnInDataframe = FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dataset}{
a data.frame containing the data
}
\item{index_column}{
a character string or a numeric/integer value indicating the column which will be used as index.
}
\item{values_column}{
a character string or a numeric/integer value indicating the column whose values will be converted into comma-separated, character strings.
}
\item{returnInDataframe}{
a logical value indicating whether to return only the converted columns or to append the character strings to the dataset.
}
}
\value{
If returnInDataframe argument is set to FALSE then a data.frame is returned with the unique values of the index column and the character strings of the values_column that correspond to each.
If returnInDataframe argument is set to TRUE then the original dataset is returned containing an extra column in which the character strings of the values_column have been appended.
}
\author{
InterMine Team
}
\examples{

# get HumanMine
im.human = initInterMine(listMines()["HumanMine"])
  
# get template query for retrieving GOTerms for specific genes
qGene_GO = getTemplateQuery(im.human, "Gene_GO")

# retrieve GOTerms for four different genes
rGene_GO = list(NULL)
for(i in seq(length(c("PPARG", "RPL5", "RPL11", "TP53")))){
  
  g = c("PPARG", "RPL5", "RPL11", "TP53")[i]
  qGene_GO$where[[1]]$value = g
  rGene_GO[[i]] = runQuery(im.human, qGene_GO)
  
}

# rbind results to data.frame
rGene_GO = do.call(rbind, rGene_GO)

# return simplified GOTerms results for each Gene
simplify_GOTerms = simplifyResult(
  dataset = rGene_GO,
  index_column = "Gene.symbol",
  values_column = "Gene.goAnnotation.ontologyTerm.identifier",
  returnInDataframe = FALSE
)

# return simplified GOTerms results for each Gene, within the original data.frame
simplify_GOTerms.2 = simplifyResult(
  dataset = rGene_GO,
  index_column = "Gene.symbol",
  values_column = "Gene.goAnnotation.ontologyTerm.identifier",
  returnInDataframe = TRUE
)

}
