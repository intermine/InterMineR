\name{newList}
\alias{newList}
\title{
Create a new list on the mine
}
\description{
Create a new list on the mine.
}
\usage{
newList(im, name, gene,  organism="H.+sapiens",  description="", timeout=30)
}
\arguments{
  \item{im}{
a list, containing the base URL and API token.
}
  \item{name}{
a string, representing the name of the list.
}
  \item{gene}{
a charater vector or a file name containing the genes in the list.
}

  \item{organism}{
a string, representing the organism which the genes are associated with.
}

  \item{description}{
a string, representing the description of the genes.
}

  \item{timeout}{
an integer, representing the number of seconds to wait for the webservice to respond.
}
}
\value{
a list, representing the response from the server for the new operation.
}
\author{
InterMine Team
}


\examples{
im <- initInterMine("humanmine.org/humanmine", "TOKEN") #replace TOKEN with your token

\dontrun{gl <- newList(im, "a", c("ABO", "ALB"))}
}
