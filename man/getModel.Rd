\name{getModel}
\alias{getModel}
\title{
Get the model of InterMine
}
\description{
Returns a representation of the data model for the mine. This describes the kind of data held, and the properties that data can have. This information can be used to build queries against that data, and to interpret the information received.
}
\usage{
getModel(im, timeout = 3)
}
\arguments{
  \item{im}{
a list containing the base URL and API token.
}

  \item{timeout}{
an integer, representing the number of seconds to wait for the webservice to respond.
}
}
\details{
The details of the data model for the various mines are available at the websites obtained by running the following command:
paste(listMines()$URL, "/tree.do", sep="")
}
\value{
a multi-level list, representating the data model for the mine.  The first-level is a list of the InterMine objects (e.g., Gene, Exon).  Each second-level list, corresponding to an InterMine object, contains three data.frame objects: attributes, references and collections.  Each attribute is a property of the InterMine object.  Each reference or collection is itself an InterMine object, acting as a member object of the InterMine object. 
}
\author{
InterMine Team
}

\examples{
  # Retrieve data model for FlyMine
  im.fly = initInterMine(listMines()["FlyMine"])

  model = getModel(im.fly)

}
