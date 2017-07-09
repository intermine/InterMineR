\name{setConstraints}
\alias{setConstraints}

\title{
setConstraints function is used to create a new or modify an existing list of constraints
}
\description{
setConstraints function is used to create or modify a list containing the constraints for an InterMine query. These constraints can be later passed on to a query formed by \code{\link{setQuery}} function. 
}
\usage{
setConstraints(
  paths,
  operators,
  values,
  modifyQueryConstraints,
  m.index
)
}

\arguments{
  \item{paths}{
a character vector defining in which bioterm type the value(s) of each constraint belong. One assigned to each constraint.
}
\item{operators}{
a character vector describing the operators for each constraint.
}
\item{values}{
a list containing the values assigned to each constraints. The path of each argument defines the type of value by which the results of the query will be filtered.
}
\item{modifyQueryConstraints}{
a list of existing contraints to be modified.
}
\item{m.index}{
a vector of integer values indicating which constraints of the list assigned to modifyQueryConstraints will be modified.
}
}
\details{
It is important to note that:

1. setConstraints assigns each argument to a constraint based on the order in which they are assigned and one at a time. This means that paths, operators and values arguments must be of the same length and carefully assigned.

2. setConstraints can create a list of new constraints or modify an existing one with the arguments modifyQueryConstraints and m.index

3. only one of the constraints can be assigned with multiple values, aiming to retrieve results for multiple bioterms (genes, etc.)

4. to assign multiple bioterms in a constraint, values argument is assigned with a vector at the appropriate order: e.g.

to assign c("a", "b") to the second constraint one must use either:

values = list(first_element, c("a", "b"), ...)

or when modifying an existing list of constraints:

values = list(c("a", "b"))
m.index = 2

}

\value{
a list containing the constraints for an InterMineR query. It can be passed to \code{\link{setQuery}} function.
}

\author{
InterMine Team
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

}

