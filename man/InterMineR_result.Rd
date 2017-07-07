\docType{class}
\name{InterMineR_result-class}
\alias{InterMineR_result-class}
\alias{InterMineR_result}

\title{InterMineR_result contains the results of a query in an InterMine instance.}

\description{
InterMineR_result is a class used to store the data retrieved after performing a query on an InterMine instance.
}
\section{Creating Objects}{
Objects can be created using the function \code{\link{runQuery2}}.
}

\section{Slots}{
  \describe{
  \item{results}{
a data.frame containing the data which were retrieved from the InterMine instance.
}
\item{constraints}{
a data.frame containing the path, op and values of each of the constraints used to perform the query on InterMine instance.
}
}
}
\details{
InterMine.result object constitutes the output value of \code{\link{runQuery2}} function, which only accepts an InterMine.query object as input.
}

\value{
An InterMineR_result object
}
\author{
InterMine Team
}
