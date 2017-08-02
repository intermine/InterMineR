\name{getTemplates}
\alias{getTemplates}
\title{
Get the information (name and title) of the templates pre-defined in InterMine
}
\description{
Get the information (name and title) of the templates pre-defined in InterMine.  A template contain a query with fixed set of output columns, and at least one editable constraint, and possibly more.
}
\usage{
getTemplates(im, format = "data.frame", timeout = 3)
}
\arguments{
  \item{im}{
a list, containing the base URL and API token.
}
  \item{format}{
a string with values being either "data.frame" or "list", representing the output format of the template information.
}

  \item{timeout}{
an integer, representing the number of seconds to wait for the webservice to respond.
}
}
\value{
a data.frame or list object, representing the information (name and title) for the pre-define templates in the mine.
}
\author{
InterMine Team
}

\examples{
# Get HumanMine template queries
im <- initInterMine(listMines()["HumanMine"])

templates <- getTemplates(im)
}
