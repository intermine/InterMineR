\name{listDatasets}
\alias{listDatasets}

\title{
Retrieve all types of InterMine features that possess dataSets as a child_name.
}
\description{
This function retrieves all types of features from the data model of each Mine, that possess dataSets as a child_name. For these types of features, there are available datasets in the respective mine instance.

}
\usage{
listDatasets(im)
}

\arguments{
  \item{im}{
a list containing the base URL and API token.
}
}
\value{
a character vector containing the all types of features from the data model of the mine, that possess dataSets as a child_name.
}
\author{
InterMine Team
}

\seealso{
\code{\link{getDatasets}}
}

\examples{
# HumanMine
#listDatasets(im = initInterMine(listMines()["HumanMine"]))

# FlyMine
#listDatasets(im = initInterMine(listMines()["FlyMine"]))

}
