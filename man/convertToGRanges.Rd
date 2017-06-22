\name{convertToGRanges}
\alias{convertToGRanges}

\title{
Convert InterMineR retrieved genomic information to objects of class \code{\link[GenomicRanges]{GRanges}}
}
\description{
convertToGRanges constitutes a wrapper function for converting genomic locations and their respective annotations to objects of the \code{\link[GenomicRanges]{GRanges}} class.
}
\usage{
convertToGRanges(
  dataset,
  seqnames,
  start, 
  end,
  names,
  strand,
  columnsAsMetadata = NULL,
  listAsMetadata = NULL,
  seqnamesInterMineChromosome = TRUE)
}

\arguments{
  \item{dataset}{
a data.frame containing all the genomic information retrieved by InterMineR. Its columns are used to create the \code{\link[GenomicRanges]{GRanges}} object.
}
\item{seqnames}{
the name of a column from dataset or a character vector of length equal to the number of rows of the dataset. Defines the values assigned as seqnames to the \code{\link[GenomicRanges]{GRanges}} object.
}
\item{start}{
the name of a column from dataset or a vector of length equal to the number of rows of the dataset. Defines the genomic coordinates assigned as start to the ranges argument of the \code{\link[GenomicRanges]{GRanges}} object.
}
\item{end}{
the name of a column from dataset or a vector of length equal to the number of rows of the dataset. Defines the genomic coordinates assigned as end to the ranges argument of the \code{\link[GenomicRanges]{GRanges}} object.
}
\item{names}{
the name of a column from dataset or a character vector of length equal to the number of rows of the dataset. Defines the values assigned as names to the ranges argument of the \code{\link[GenomicRanges]{GRanges}} object.
}
\item{strand}{
the name of a column from dataset or a character vector of length equal to the number of rows of the dataset. Defines the values assigned as strand information to the \code{\link[GenomicRanges]{GRanges}} object.
}
\item{columnsAsMetadata}{
a character vector containing the names of the dataset columns that are passed as metadata in the \code{\link[GenomicRanges]{GRanges}} object.
}
\item{listAsMetadata}{
a list of vectors, each of which has length equal to the number of rows of the dataset. The values of the list are passed as metadata to the \code{\link[GenomicRanges]{GRanges}} object.
}
\item{seqnamesInterMineChromosome}{
a logical value indicating whether the values passed as seqnames are InterMineR chromosome.primaryIdentifiers (e.g. "2R", "3R", "X") or not.
}
}
\details{
The InterMineR package provides a flexible interface to the \code{\link[http://intermine.org/]{InterMine}} web services, which allow for rapid retrieval of various genomic information. 

convertToGRanges function is designed to facilitate the conversion of genomic locations and their respective annotations, in the format by which they are retrieved using InterMineR, to an object of the \code{\link[GenomicRanges]{GRanges}} class.
}
\value{
An object of the \code{\link[GenomicRanges]{GRanges}} class containing genomic locations and annotations retrieved by InterMineR queries.
}
\references{
\code{\link[https://bioconductor.org/packages/release/bioc/vignettes/GenomicRanges/inst/doc/GenomicRangesIntroduction.pdf]{An Introduction to the GenomicRanges Package}}
}
\author{
InterMine Team
}
\examples{
# get FlyMine
im.fly = initInterMine(listMines()["FlyMine"])

# modify template query for Transcription Factor (TF) Binding sites
qTF_Binding = getTemplateQuery(im.fly,"ChromLocation_TFBindingSiteLocationGeneFactor")

qTF_Binding$where[[4]]$value = "1000000"
qTF_Binding$where[[5]]$value = "20000000"

rTF_Binding = runQuery(im.fly, qTF_Binding)

# assign random values for strand of the genomic location retrieved, in InterMine format
rTF_Binding$gene.strand = sample(c("1", "-1", ""), nrow(rTF_Binding), replace = TRUE)

# convert to GRanges object
test = convertToGRanges(
  dataset = rTF_Binding,
  seqnames = rTF_Binding$TFBindingSite.chromosome.primaryIdentifier,
  names = rTF_Binding$TFBindingSite.factor.name,
  start = rTF_Binding$TFBindingSite.chromosomeLocation.start,
  end = rTF_Binding$TFBindingSite.chromosomeLocation.end,
  strand = "gene.strand",
  columnsAsMetadata = c("TFBindingSite.gene.regulatoryRegions.dataSets.dataSource.name",
                        "TFBindingSite.factor.primaryIdentifier"),
  listAsMetadata = list(c(factor.primaryIdentifier = rTF_Binding$TFBindingSite.factor.primaryIdentifier))
)

# check results
test

}
