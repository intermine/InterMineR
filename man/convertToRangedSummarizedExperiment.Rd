\name{convertToRangedSummarizedExperiment}
\alias{convertToRangedSummarizedExperiment}

\title{
Convert experimental results retrieved by InterMineR queries to an object of the \code{\link[SummarizedExperiment]{RangedSummarizedExperiment}} class
}
\description{
convertToRangedSummarizedExperiment constitutes a wrapper function for converting genomic information and experimental from \code{\link[http://intermine.org/]{InterMine}} to an object of the \code{\link[SummarizedExperiment]{RangedSummarizedExperiment}} class. 
}
\usage{
convertToRangedSummarizedExperiment(
  dataset,
  SampleColumn,
  GeneColumn,
  ValueColumn,
  OrganismValue,
  colsForSampleMetadata,
  exonsForRowRanges = FALSE)
}

\arguments{
\item{im}{
a list containing the base URL and API token.
}
\item{dataset}{
a data.frame retrieved with InterMineR queries that contains experimental data from high-throughput assays.
}
\item{SampleColumn}{
a character string or an integer indicating which column of the dataset contains the samples.
}
\item{GeneColumn}{
a character string or an integer indicating which column of the dataset contains the genes.
}
\item{ValueColumn}{
a character string or an integer indicating which column of the dataset contains the experimental data.
}
\item{OrganismValue}{
a character string with the name of the organism from which the genomic information are retrieved.
}
\item{colsForSampleMetadata}{
an integer vector indicating the columns of the dataset which will be assigned as sample metadata in the colData argument of the \code{\link[SummarizedExperiment]{RangedSummarizedExperiment}}.
}
\item{exonsForRowRanges}{
a logical value indicating whether the rowRanges argument of the \code{\link[SummarizedExperiment]{RangedSummarizedExperiment}} should be assigned with a GRangeslist containing only annotations about the genes (default) or annotations about all exons of each gene.
}
}
\details{
The InterMineR package provides a flexible interface to the \code{\link[http://intermine.org/]{InterMine}} web services, which allow for rapid retrieval of various genomic information.

convertToRangedSummarizedExperiment function facilitates the conversion of genomic information and experimental data from high-throughput assays, which are retrieved by using InterMineR queries, to an object of the \code{\link[SummarizedExperiment]{RangedSummarizedExperiment}} class.

It is noteworthy that the \code{\link[stats]{reshape}} function is used to convert the experimental data from the InterMineR format (long format) to the matrix (wide format) assigned to the assays argument of the \code{\link[SummarizedExperiment]{SummarizedExperiment}}.

}
\value{
an object of the \code{\link[SummarizedExperiment]{RangedSummarizedExperiment}} class containing genomic information and experimental data of high-throughput assays, which are retrieved with the InterMineR queries system.
}
\references{
\code{\link[https://bioconductor.org/packages/release/bioc/vignettes/SummarizedExperiment/inst/doc/SummarizedExperiment.html]{SummarizedExperiment for Coordinating Experimental Assays, Samples, and Regions of Interest}
}
}

\author{
InterMine Team
}

\examples{

# 10 Drosophila melanogaster genes of interest
Drosophila.genes = c("BEAF-32", "Antp", "bcd", "caup", "tup", "E2f2", "dsx", "so", "toy", "Lim1")

# retrieve microarray time course experimental data for Drosophila.genes
# get FlyMine instance
im.fly = initInterMine(listMines()["FlyMine"])

# get FlyMine microarray time course template query
queryForData = getTemplateQuery(im.fly, "Gene_TimeCourseExpression")

test.data = list(NULL)
ind.null = c()

for(i in seq(length(Drosophila.genes))){
  # set value in gene constraint
  queryForData$where[[3]]$value = as.character(Drosophila.genes[i])
  
  # run query and save the results of genes that exist in the Microarray time course dataset
  r = runQuery(im.fly, queryForData)
  
  ind.null = c(ind.null, is.null(r))
  
  test.data[[i]] = r
  
}

# remove genes for which no experimental data were retrieved
test.data = test.data[which(!ind.null)]

# rbind data together
test.data = do.call(rbind, test.data)

# using integer index for columns in arguments
test1 = convertToRangedSummarizedExperiment(
  im = im.fly,
  dataset = test.data,
  SampleColumn = 2,
  GeneColumn = 1,
  ValueColumn = 3,
  OrganismValue = "Drosophila melanogaster",
  colsForSampleMetadata = 4:7,
  exonsForRowRanges = TRUE
)

test1

# using directly column names in arguments
test2 = convertToRangedSummarizedExperiment(
  im = im.fly,
  dataset = test.data,
  SampleColumn = "Gene.microArrayResults.assays.sample2",
  GeneColumn = "Gene.symbol",
  ValueColumn = "Gene.microArrayResults.value",
  OrganismValue = "Drosophila melanogaster",
  colsForSampleMetadata = 4:7,
  exonsForRowRanges = TRUE
)

test2
