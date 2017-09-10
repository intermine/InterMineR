\docType{class}

\name{InterMineR-class}

\alias{InterMineR-class}

\title{
InterMineR class contains the input values for performing queries in an InterMine instance.
}

\description{
InterMineR constitutes a class used to store the information which are required for performing a query for biological data in an InterMine instance. Specifically, it contains information about:

1) the type of data which are to be returned from the InterMine instance, 

2) the type of sorting performed on these data, and 

3) the constraints used to perform the query for the data of interest.

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

\section{Details}{
InterMineR class specifies an object in which the input values of a query can be stored. A single constraint within the object can be assigned with multiple values.
}

\author{
InterMine Team
}

\seealso{
  \code{\link{setConstraints}}, \code{\link{setQuery}}, \code{\link{InterMineR-methods}}
}
