\name{renameList}
\alias{renameList}
\title{
Rename a list stored on the mine
}
\description{
Rename a list stored on the mine.
}
\usage{
renameList(im, old.name, new.name, timeout = 3)
}
\arguments{
  \item{im}{
a list, containing the base URL and API token.
}
  \item{old.name}{
a string, representing the old name of the list.
}
  \item{new.name}{
a string, representing the new name of the list.
}

  \item{timeout}{
an integer, representing the number of seconds to wait for the webservice to respond.
}
}
\value{
a list, representing the response from the server for the rename operation.
}
\author{
InterMine Team
}


\examples{
im <- initInterMine("humanmine.org/humanmine", "TOKEN") #replace TOKEN with your token

\dontrun{status <- renameList(im, "a", "b")}
}
