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
A data frame object containing two columns:
\item{URL}{the base URL of the webservice of the database}
\item{name}{the name of the database}
}
\references{
http://intermine.org
}
\author{
InterMine Team
}


\examples{
listMines()
}
