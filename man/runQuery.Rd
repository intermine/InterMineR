\name{runQuery}
\docType{methods}

\alias{runQuery}
\alias{runQuery,ANY,InterMineR-method}
\alias{runQuery,ANY,list-method}

\title{
Run InterMineR queries
}
\description{
Returns results from a query against data held inside the mine. These queries are similar to SQL queries, in that they request certain defined output columns of output, filtering the results through a series of "constraints".
}
\usage{
runQuery(im, qry, timeout = 60)
}

\arguments{
   \item{im}{
a Service object containing the base URL and API token, created by \code{\link{initInterMine}}. 
}

  \item{qry}{
an InterMineR or a list object, representing the query to the database.
}

  \item{timeout}{
an integer, representing the number of seconds to wait for the web service to respond.
}
}
\details{
Use \code{\link{setQuery}} function to create an InterMineR object. For setting a single constraint with multiple values, the function \code{\link{setConstraints}} can be used.

Alternatively, the user can define manually the constraints, the selection of data to be returned and the value by which they are ordered, as a list object.

For more information checkout the vignette of the package.
}

\value{
a data.frame containing the data which were retrieved from the InterMine instance.
}

\section{Methods}{
\describe{

# S4 method for class 'InterMineR'
\item{\code{runQuery(im, qry = "InterMineR")}}{
The method accepts an object of the class InterMineR and uses its information to perform the query on the defined InterMine instance.

\code{\link{InterMineR-class}} objects can contain a single constraint with multiple values.
}

# S4 method for class 'list'
\item{\code{runQuery(im, qry = "list")}}{
The method accepts an object of the class list and uses its information to perform the query on the defined InterMine instance.

Queries defined as lists can possess only constraints with one value.
}
}}

\author{
InterMine Team
}

\seealso{
\code{\link{setConstraints}}, \code{\link{setQuery}}, \code{\link{InterMineR-class}}, \code{\link{newQuery}}
}

\examples{
# 1. Adapt 'GO_Gene' template query from FlyMine to 'InterMineR'

# get FlyMine instance
im.fly = initInterMine(listMines()["FlyMine"])

# get GO_Gene template query
qGO_Gene = getTemplateQuery(im.fly, "GO_Gene")

# constraint with GO value
qGO_Gene$where[[3]]

# modify GO_Gene template query to have more than one GO values
go.constraints = setConstraints(
  values = list(c("DNA repair", "cellular response to DNA damage stimulus")),
  modifyQueryConstraints = qGO_Gene,
  m.index = 3
)

go.constraints[[3]]

# create 'InterMineR' object
go.query = setQuery(
  inheritQuery = qGO_Gene,
  where = go.constraints
)

go.query

# run InterMineR query
go.results = runQuery(
  im = im.fly,
  qry = go.query
)

head(go.results)

# 2. Create similar query manually for Homo sapiens, using HumanMine

# get HumanMine instance
im.human = initInterMine(listMines()["HumanMine"])

# create constraints using GO terms and organism as values
hsa.go.constraints = setConstraints(
  paths = c("Gene.goAnnotation.ontologyTerm.parents.name",
            "Gene.organism.name"),
  operators = rep("=", 2),
  values = list(c("DNA repair", "cellular response to DNA damage stimulus"),
                  "Homo sapiens")
)

hsa.go.constraints

# create 'InterMineR' object
hsa.go.query = setQuery(
  select = c("Gene.secondaryIdentifier",
             "Gene.symbol",
             "Gene.goAnnotation.ontologyTerm.parents.name",
             "Gene.goAnnotation.ontologyTerm.parents.identifier",
             "Gene.goAnnotation.ontologyTerm.name",
             "Gene.goAnnotation.ontologyTerm.identifier"),
  orderBy = list(c(Gene.secondaryIdentifier = "ASC")),
  where = hsa.go.constraints
)

hsa.go.query

# run InterMineR query
hsa.go.results = runQuery(
  im = im.human,
  qry = hsa.go.query
)

head(hsa.go.results)

}
