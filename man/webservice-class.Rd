\docType{class}

\name{Service-class}

\alias{webservice-class}

\title{
Service class represents connections to different InterMine WebServices
}

\description{
The Service class is the main interface for the user. It will provide access to queries and templates, as well as doing the background task of fetching the data model, and actually requesting the query results.
}

\section{Creating Objects}{
Objects can be created using the function \code{initInterMine}.
}

\section{Slots}{
  \describe{
  \item{mine}{
the URL base. 
}
  }
}

\author{
InterMine Team
}

\seealso{
  \code{\link{InterMineR}}, \code{\link{webservice-class}}, \code{\link{ListManager-methods}}
}
