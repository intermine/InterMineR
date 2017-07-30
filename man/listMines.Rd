\name{listMines}
\alias{listMines}
\title{
List the available InterMine-powered databases
}
\description{
InterMine-powered databases such as FlyMine, modENCODE, RatMine, YeastMine, HumanMine and TargetMine are integrated databases of genomic, expression and protein data for various organisms.  The function listMines() lists the current available databases.
}
\usage{
listMines()
}
\value{
A character vector containing the name and the base URL of the web service of the database.
}
\references{
\url{http://registry.intermine.org/} 
}
\author{
InterMine Team
}

\examples{
listMines()
}
