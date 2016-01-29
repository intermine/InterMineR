\name{runQuery}
\alias{runQuery}
\title{
Run a query
}
\description{
Returns results from a query against data held inside the mine. These queries are similar to SQL queries, in that they request certain defined output columns of output, filtering the results through a series of "constraints". 
}
\usage{
runQuery(im, qry, format="data.frame", timeout = 60)
}
\arguments{
  \item{im}{
a list, containing the base URL and API token.
}

  \item{qry}{
a list or XML string, representing the query to the database. 
}
  \item{format}{
a string, representing the output format of the query result.  It could be one of these values: "data.frame", "sequence". 
}

  \item{timeout}{
an integer, representing the number of seconds to wait for the webservice to respond.
}
}
\value{
If format="data.frame", the function returns a data.frame object, representing the query result.

If format="sequence", the function returns an XStringSet object, representing the DNA sequences resulted from the query.
}
\author{
InterMine Team
}


\examples{
im <- initInterMine("humanmine.org/humanmine")

queryGeneIden <- getTemplateQuery(im, "Gene_Identifiers")
queryGeneIden$view
queryGeneIden$constraints

qryRes <- runQuery(im, queryGeneIden)
}
