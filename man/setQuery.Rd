\name{setQuery}
\alias{setQuery}

\title{
Initialize a new InterMineR query or modify an existing list query
}
\description{
setQuery function is used to create a query for an InterMine instance. It can also accept and modify pre-defined queries of class list, like the template queries which can be retrieved with the function \code{\link{getTemplateQuery}}. It returns a defined query stored as an object of class 'InterMineR'.
  
A query needs to have at least view, constraints and constraintLogic. The view is a vector containing the columns of the query output. The constraint is a matrix containing the following columns: path (the path of the constraint), op (the constraint operator, one of '=', '!=', 'LOOKUP', 'ONE OF', 'NONE OF', '>', '<', '>=', '<=', 'LIKE'), value (the constraint value), code (the name of the constraint), extraValue (optional, required for LOOKUP constraints). The constraintLogic by default is "AND" operation, e.g., "A and B", where A and B are the codes in the constraints.
}
\usage{
setQuery(
  select,
  orderBy,
  where,
  name = "",
  description = "",
  inheritQuery
)
}

\arguments{
  \item{select}{
a character vector defining the type of data to be returned.
}
\item{orderBy}{
a list the name of the column and the type of sorting which will be used to order the retrieved data.frame.
}
\item{where}{
a list containing the constraints used to restrict the query. Each constraint constitutes a list as well. If one wishes for a single constraint to possess more than one values, the \code{\link{setConstraints}} function can be used.
}
\item{name}{
Assign with a character string giving a name to the query. Pre-fixed with "".
}
\item{description}{
Assign with a character string describing the purpose and the type of data retrieved by the query. Pre-fixed with "".
}
\item{inheritQuery}{
Assign with a pre-defined query list, the input values of which can be modified and inherited in the new query object of class 'InterMineR'.
}
}
\details{
setQuery function can be used to create queries with a single constraint containing multiple values. The constraints can be first defined with the \code{\link{setConstraints}} function. The resulting 'InterMineR' object is passed to the \code{\link{runQuery}} function to perform the query.
}
\value{
An InterMineR object
}

\author{
InterMine Team
}

\seealso{
\code{\link{setConstraints}}, \code{\link{runQuery}}, \code{\link{InterMineR-class}}
}

\examples{
# get mine instance
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

class(go.query)
go.query

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

class(hsa.go.query)
hsa.go.query

}
