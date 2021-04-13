\name{getDatasets}
\alias{getDatasets}

\title{
Retrieve information about the available datasets of a Mine instance.
}
\description{
This function retrieves information about the available datasets of a mine instance. It is necessary to specify the type of feature (e.g. Gene, TFBindingSite) for this mine, and a value of this feature OR of one of its child_name values.
}
\usage{
getDatasets(
  im, 
  type,                      
  child_name,    
  value,
  op
)
}

\arguments{
  \item{im}{
a Service object containing the base URL and API token, created by \code{\link{initInterMine}}. 
}
\item{type}{
a character string defining the type of feature for which the information about the available datasets will be retrieved. Use the \code{\link{listDatasets}} function to retrieve types with available datasets on each mine.
}
\item{child_name}{
a character string defining the child_name values of type, which will be used to retrieve the information about the available datasets. Use \code{\link{getModel}} function to retrieve the available child_name values for each type.
}
\item{value}{
a character string defining the value of the child_name.
}
\item{op}{
a character string defining the op argument.
}
}
\details{

The getDatasets function uses an InterMineR query to retrieve the information of the available datasets for:

1. a specific mine (e.g. FlyMine)

2. a specific type of feature (e.g. Gene, TFBindingSite) for this mine, and

3. a specific value of this feature OR of one of its child_name values.

For all genes of Drosophila melanogaster (Gene.organism = Drosophila melanogaster) OR for a specific gene (Gene LOOKUP PPARG)

}
\value{

a data.frame containing information about about the available datasets for a specific type of feature (e.g. Gene) on a specific mine instance (HumanMine, FlyMine, etc.).

}

\author{
InterMine Team
}

\seealso{
\code{\link{listDatasets}}, \code{\link{getModel}}
}

\examples{
# Define mines
im.fly = initInterMine(listMines()["FlyMine"])
im.human = initInterMine(listMines()["HumanMine"])

# get information about all available datasets of Drosophila melanogaster genes
dme_gene_datasets = getDatasets(
  im = im.fly,
  type = "Gene",
  child_name = "organism.name",
  value = "Drosophila melanogaster",
  op = "="
)

# get available datasets for PPARG gene from FlyMine
pparg_related = getDatasets(
  im = im.fly,
  type = "Gene",
  value = "PPARG",
  op = "LOOKUP"
)

# get available datasets for PPARG gene from HumanMine
pparg_related.2 = getDatasets(
  im = im.human,
  type = "Gene",
  value = "PPARG",
  op = "LOOKUP"
)

}
