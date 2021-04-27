\name{list_manager}
\docType{methods}

\alias{webservice-methods}
\alias{list_manager}
\alias{list_manager-methods}
\alias{list_manager,webservice-methods}

\title{
A Class for Managing List Content and Operations
}
\description{
This class may be called itself, and all the useful methods it has are available through the list objects themselves.
}
\usage{
list_manager(Service-class)
}

\arguments{
  \item{Service-class}{
a \code{Service-class} object containing the base URL and API token.
}  
}

\details{
The public gene lists for each Mine are available at the websites obtained by the following command:
paste0(listMines()["Mine_Name"], "/bag.do?subtab=view")

Get a new ListManager to use with this service. This class provides methods to manage list contents and operations.
}
\value{
list_manager function returns an object containing the following values:

\item{DEFAULT_LIST_NAME}{"my_list"}

\item{DEFAULT_DESCRIPTION}{"List created with R client library"}

\item{URL endpoints}{LIST_PATH, INTERSECTION_PATH, UNION_PATH, DIFFERENCE_PATH, SUBTRACTION_PATH}

\item{mine}{the base URL}

\item{token}{the API token}

}

\author{
InterMine Team
}

\seealso{
\code{\link{ListManager-methods}}, \code{\link{ListManager-class}}
}

\examples{
#hashed out example as requires API key
#im.human.list <- list_manager(initInterMine(listMines()["HumanMine"],"TOKEN")) #Add your API Token key
#class(im.human.list)
#resp <- get_list(im.human.list, " PL_Pax6_Targets")
#resp
}
