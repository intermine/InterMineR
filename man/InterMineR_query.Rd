\docType{class}
\name{InterMineR_query-class}
\alias{InterMineR_query-class}
\alias{InterMineR_query}

\title{
InterMineR_query class contains the input values for performing queries in an InterMine instance.
}
\description{
InterMineR_query is a class used to store the input values for performing a query for biological data in an InterMine instance. Specifically, it contains the type of data to be returned from InterMine, the type of sorting and the constraints used to perform the query for the data of interest.
}
\section{Creating Objects}{
Objects can be created using the function \code{\link{setQuery}}.
}

\section{Slots}{
  \describe{
  \item{name}{
Assign with a character string giving a name to the query. Pre-fixed with "".
}
\item{description}{
Assign with a character string describing the purpose and the type of data retrieved by the query. Pre-fixed with "".
}
\item{select}{
a character vector defining the type of data to be returned.
}
\item{orderBy}{
a list the name of the column and the type of sorting which will be used to order the retrieved data.frame. 
}
\item{where}{
a list containing the constraints used to restrict the query. Each constraint constitutes a list as well.
}
}
}
\details{
InterMine.query class specifies an object in which the input values of a query, containing a contraint with multiple values, can be stored. This way queries in InterMineR are stored in a compact form and at the same time the creation of multiple lists and iteration loops is avoided.
}
\value{
An InterMineR_query object
}
\author{
InterMine Team
}

