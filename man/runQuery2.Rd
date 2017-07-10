\name{runQuery2}
\alias{runQuery2}

\title{
runQuery2 function for running a query on InterMine with an 'InterMineR_query' object
}
\description{
Much like \code{\link{runQuery}}, runQuery2 function performs a query on an InterMine instance using an 'InterMineR_query' object as input instead of a list query. 
}
\usage{
runQuery2(
  im,
  qry,
  timeout=60
)
}

\arguments{
    \item{im}{
a list, containing the base URL and API token.
}

  \item{qry}{
an 'InterMineR_query' object, representing the query to the database.
}

  \item{timeout}{
an integer, representing the number of seconds to wait for the webservice to respond.
}
}
\details{
Use \code{\link{setQuery}} function to create an 'InterMineR_query' object. For setting a single constraint with multiple values, the function \code{\link{setConstraints}} can be used. The results from the query on InterMine instance are stored in an 'InterMineR_result' object.
}
\value{
An InterMineR_result object:
\item{results}{
a data.frame containing the data which were retrieved from the InterMine instance.
}
\item{constraints}{
a data.frame containing the path, op and values of each of the constraints used to perform the query on InterMine instance.
}
}

\author{
InterMine Team
}

\examples{

# 1. Adapt 'GO_Gene' template query from FlyMine to 'InterMineR_query' and 'InterMineR_result'

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

# create 'InterMineR_query' object
go.query = setQuery(
  inheritQuery = qGO_Gene,
  where = go.constraints
)

go.query

# run query
go.results = runQuery2(
  im = im.fly,
  qry = go.query
)

head(go.results@result)
go.results@constraints

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

# create 'InterMineR_query' object
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

# run 'InterMineR_query'
hsa.go.results = runQuery2(
  im = im.human,
  qry = hsa.go.query
)

head(hsa.go.results@result)
hsa.go.results@constraints

}

