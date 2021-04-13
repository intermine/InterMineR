\name{getVersion}
\alias{getVersion}
\title{
Get the version information of InterMine
}
\description{
Returns an integer representing the capabilities of the webservice.
}
\usage{
getVersion(im, timeout = 3)
}
\arguments{
  \item{im}{
a Service object containing the base URL and API token, created by \code{\link{initInterMine}}. 
}
  \item{timeout}{
an integer, representing the number of seconds to wait for the webservice to respond.
}
}
\value{
an integer, representing the capabilities of the webservice.
}
\author{
InterMine Team
}


\examples{

  im <- initInterMine(listMines()["HumanMine"])

  getVersion(im)
  
}
