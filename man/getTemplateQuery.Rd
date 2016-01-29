\name{getTemplateQuery}
\alias{getTemplateQuery}
\title{
Get the query contained in a template
}
\description{
Get a template query for a mine.  A template contain a saved query with a view and constraint.  The user can modify this query to obtain the desired result.  The view is a vector containing the output columns of the query.  The constraint is a matrix containing the following columns: path (the path of the constraint), op (the constraint operator, one of '=', '!=', 'LOOKUP', 'ONE OF', 'NONE OF', '>', '<', '>=', '<=', 'LIKE'), value (the constraint value),  code (the name of the constraint), extraValue (optional, required for LOOKUP constraints). 
}
\usage{
getTemplateQuery(im, name, timeout=3)
}
\arguments{
  \item{im}{
a list, containing the base URL and API token.
}
  \item{name}{
a string, representing the name of the pre-defined template.
}

  \item{timeout}{
an integer, representing the number of seconds to wait for the webservice to respond.
}
}
\value{
a list, reprenting the query contained in the pre-defined template.  The list should contain at least two elements, view and constrain.
}
\author{
InterMine Team
}


\examples{
im <- initInterMine("humanmine.org/humanmine")

queryGeneIden <- getTemplateQuery(im, "Gene_Identifiers")
}
