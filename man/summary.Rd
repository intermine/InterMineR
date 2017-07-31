\name{summary}
\docType{methods}
\alias{summary}
\alias{summary,ANY-method}
\alias{summary,InterMineR-method}

\title{
Summarize InterMineR query constraints
}

\description{
Summarize the information about the constraints contained by an object of the class 'InterMineR'.
}

\usage{
summary(object,...)
}

\arguments{
  \item{object}{
  an object of the class 'InterMineR'.
}
  \item{...}{
  additional arguments affecting the summary produced.
}
}

\value{
a data.frame containing the constraints of the InterMineR object as rows. Each constraint is constituted by a path, an operator and one or more values. Multiple values are returned as a comma-separated character string.
}

\author{
InterMine Team
}

\seealso{
\code{\link{InterMineR-class}}, \code{\link{setQuery}}
}

\examples{
# get FlyMine instance
im.fly = initInterMine(listMines()["FlyMine"])

# get GO_Gene template query
qGO_Gene = getTemplateQuery(im.fly, "GO_Gene")

# modify GO_Gene template query to have more than one GO values
go.constraints = setConstraints(
  values = list(c("DNA repair", "cellular response to DNA damage stimulus")),
  modifyQueryConstraints = qGO_Gene,
  m.index = 3
)

# create 'InterMineR' object
go.query = setQuery(
  inheritQuery = qGO_Gene,
  where = go.constraints
)

# get InterMineR constraint summary
summary(go.query)

}
