\name{initInterMine}
\alias{initInterMine}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Initialize the list containing the base URL and API token.
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Initialize the InterMine list with the base URL of the webservice of the database and the API token.

Some resources such as lists are normally privately associated with the individual user that created them and require authentication for access.  To access these private resources, each request needs to be authenticated, using an API key token.  You can get an API token from the web-app of the service you intend to access: visit the MyMine tab after logging-in and click on API Key.
}
\usage{
initInterMine(mine = listMines()["HumanMine"], token="")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mine}{
%%     ~~Describe \code{mine} here~~
a string, representing the base URL of the webservice of the database.
}
  \item{token}{
%%     ~~Describe \code{token} here~~
a string, representing the API token in order to use private functions such as list and enrichment.
}
}
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A list, containing the base URL and API token.
}
\references{
%% ~put references to the literature/web site here ~
http://intermine.org/wiki/WebService#APIKeyTokens
}
\author{
%%  ~~who you are~~
InterMine Team
}
%\note{
%%  ~~further notes~~
%}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
%}
\examples{
im <- initInterMine(mine = listMines()["HumanMine"], "TOKEN") #replace TOKEN with your token
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
