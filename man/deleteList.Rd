\name{deleteList}
\alias{deleteList}
\title{
Delete a list stored on the mine
}
\description{
Delete a list stored on the mine.
}
\usage{
deleteList(im, name, timeout=3)
}
\arguments{
  \item{im}{
a list containing the base URL and API token.
}
  \item{name}{
a string, representing the name of the list to delete.
}

  \item{timeout}{
an integer, representing the number of seconds to wait for the webservice to respond.
}
}
\value{
a list, represetning the response from the server for the delete operation.
}
\author{
InterMine Team
}

\examples{
#im <- initInterMine("humanmine.org/humanmine", "TOKEN") #replace TOKEN with your token

\dontrun{status <- deleteList(im, "a")} # "a" is the name of a list
}

