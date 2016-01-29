\name{getLists}
\alias{getLists}
\title{
Get the information of the lists stored on the mine
}
\description{
Returns information on the lists the user has access to in the mine.
}
\usage{
getLists(im, timeout = 3)
}
\arguments{
  \item{im}{
a list, containing the base URL and API token.
}
  \item{timeout}{
an integer, representing the number of seconds to wait for the webservice to respond.
}
}
\value{
a character vector, representing the names of the lists on the mine.
}
\author{
InterMine Team
}


\examples{
im <- initInterMine("humanmine.org/humanmine", "TOKEN") #replace TOKEN with your token

gl <- getLists(im)
}
