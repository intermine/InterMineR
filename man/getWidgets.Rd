\name{getWidgets}
\alias{getWidgets}

\title{
Get the widgets of InterMine
}
\description{
Returns a representation of the available widgets for the mine. Each entry for the widget includes details of its type, the kind of data it processes, and any filters it accepts.
}
\usage{
getWidgets(im)
}

\arguments{
  \item{im}{
a Service object containing the base URL and API token, created by \code{\link{initInterMine}}. 
}
}

\value{
A data.frame containing information about the widgets that are available for each mine.
}

\author{
InterMine Team
}

\note{
The names of the widgets of the enrichment type can be passed as arguments to \code{\link{doEnrichment}} function.
}

\seealso{
\code{\link{getGeneIds}}, \code{\link{doEnrichment}}
}

\examples{
# Get available FlyMine widgets
FlyMine.widgets <- getWidgets(im = initInterMine(mine = listMines()["FlyMine"]))
# Get available HumanMine widgets
HumanMine.widgets <- getWidgets(im = initInterMine(mine = listMines()["HumanMine"]))
}
