\docType{class}

\name{ListManager-class}

\alias{ListManager-class}

\title{
ListManager class provides methods to manage list contents and operations.
}

\description{
ListManager constitutes a class used to store the information required for managing lists contents and performing operations. Specifically, it contains information about:

1) the default list name and description, 

2) the different URL endpoints, and 

3) the information of the WebService. 
}

\section{Creating Objects}{
Objects can be created using the function \code{\link{list_manager}}, which is a webservice method.
}

\section{Slots}{
  \describe{
  \item{name}{
Assign with a character string giving a name to the query. Pre-fixed with "".
}
\item{DEFAULT_LIST_NAME}{
Assign with a character string, it is used when the names is not specified or the list exists.
}
\item{DEFAULT_DESCRIPTION}{
a character string that indicates that the list is created with the R client library.
}
\item{LIST_PATH}{
URL endpoint for storing lists. 
}
\item{INTERSECTION_PATH}{
URL endpoint for intersecting lists.
}
\item{UNION_PATH}{
URL endpoint for the union of lists. 
}
\item{DIFFERENCE_PATH}{
URL endpoint for the difference of lists. 
}
\item{SUBTRACTION_PATH}{
URL endpoint for the subtraction lists. 
}
\item{mine}{
URL of the an InterMine Webservice. 
}
\item{token}{
API access key. 
    }
  }
}

\section{Details}{
ListManager class specifies an object in which the the common inputs to make an API request are stored.
}

\author{
InterMine Team
}

\seealso{
  \code{\link{list_manager}}, \code{\link{ListManager-methods}}, \code{\link{webservice-methods}}
}