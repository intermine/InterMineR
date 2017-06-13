\name{getRelease}
\alias{getRelease}
\title{
Get the current release information of InterMine
}
\description{
Returns a string describing the release of the mine.
}
\usage{
getRelease(im, timeout = 3)
}
\arguments{
  \item{im}{
a list containing the base URL and API token.
}
  \item{timeout}{
an integer, representing the number of seconds to wait for the webservice to respond.
}
}
\value{
a string, describing the release of the mine.
}
\author{
InterMine Team
}


\examples{

  im <- initInterMine(listMines()["HumanMine"])

  getRelease(im)
  
}
