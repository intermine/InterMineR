\name{newQuery}
\alias{newQuery}
\title{
Initialize a new query
}
\description{
A query needs to have at least view, constraints and constraintLogic.  The view is a vector containing the columns of the query output.  The constraint is a matrix containing the following columns: path (the path of the constraint), op (the constraint operator, one of '=', '!=', 'LOOKUP', 'ONE OF', 'NONE OF', '>', '<', '>=', '<=', 'LIKE'), value (the constraint value),  code (the name of the constraint), extraValue (optional, required for LOOKUP constraints).  The constraintLogic by default is "AND" operation, e.g., "A and B", where A and B are the codes in the constraints.
}
\usage{
newQuery(name="", view=character(), sortOrder="", longDescription="",
      constraintLogic=NULL)
}
\arguments{
  \item{name}{
  a string, representing the name of the query.
}
  \item{select}{
  a character vector, representing the fields to be selected from InterMine.
}
  \item{orderBy}{
  a string, representing the field according to which the query result is sorted and the sort order ("asc" or "desc"), following the format "FIELD ORDER".
}
  \item{longDescription}{
a string, representing the description of the query.
}
  \item{constraintLogic}{
a string, representing the logical relationship between the constraints, e.g., "A or B" where "A" and "B" are the codes in the constraints.
}
}
\value{
a list, representing the query.
}
\author{
InterMine Team
}


\examples{
nq <-  newQuery()
}
