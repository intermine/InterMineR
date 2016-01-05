\name{getLists}
\alias{getLists}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Get the information of the lists stored on the mine
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Returns information on the lists the user has access to in the mine.
}
\usage{
getLists(im, timeout = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{im}{
%%     ~~Describe \code{im} here~~
a list, containing the base URL and API token.
}
  \item{timeout}{
%%     ~~Describe \code{timeout} here~~
an integer, representing the number of seconds to wait for the webservice to respond.
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
a character vector, representing the names of the lists on the mine.
}
%\references{
%% ~put references to the literature/web site here ~
%}
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
im <- initInterMine("humanmine.org/humanmine", "TOKEN") #replace TOKEN with your token

gl <- getLists(im)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
