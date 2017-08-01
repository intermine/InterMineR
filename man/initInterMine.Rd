\name{initInterMine}
\alias{initInterMine}
\title{
Initialize the list containing the base URL and API token.
}
\description{
Initialize the InterMine list with the base URL of the webservice of the database and the API token.

Some resources such as lists are normally privately associated with the individual user that created them and require authentication for access.  To access these private resources, each request needs to be authenticated, using an API key token.  You can get an API token from the web-app of the service you intend to access: visit the MyMine tab after logging-in and click on API Key.
}
\usage{
initInterMine(mine = listMines()["HumanMine"], token="")
}
\arguments{
  \item{mine}{
a string, representing the base URL of the webservice of the database.
}
  \item{token}{
a string, representing the API token in order to use private functions such as list and enrichment.
}
}
\value{
A list containing the base URL and API token.
}
\references{
/url{http://intermine.readthedocs.io/en/latest/web-services/}
}
\author{
InterMine Team
}

\examples{
im <- initInterMine(mine = listMines()["HumanMine"], "TOKEN") #replace TOKEN with your token
}
