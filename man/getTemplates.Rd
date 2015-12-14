\name{getTemplates}
\alias{getTemplates}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Get the information (name and title) of the templates pre-defined in InterMine
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Get the information (name and title) of the templates pre-defined in InterMine.  A template contain a query with fixed set of output columns, and at least one editable constraint, and possibly more.
}
\usage{
getTemplates(im, format = "data.frame", timeout = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{im}{
%%     ~~Describe \code{im} here~~
a list, containing the base URL and API token.
}
  \item{format}{
%%     ~~Describe \code{format} here~~
a string with values being either "data.frame" or "list", representing the output format of the template information.
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
a data.frame or list object, representing the information (name and title) for the pre-define templates in the mine.
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
im <- initInterMine("metabolicmine.org/beta")

template <- getTemplates(im)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
