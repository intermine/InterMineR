\name{getModel}
\alias{getModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Get the model of InterMine
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Returns a representation of the data model for the mine. This describes the kind of data held, and the properties that data can have. This information can be used to build queries against that data, and to interpret the information received.
}
\usage{
getModel(im, timeout = 3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{im}{
%%     ~~Describe \code{im} here~~
a list containing the base URL and API token.
}
%%  \item{format}{
%%     ~~Describe \code{format} here~~
%%a string with values being either "list", "xml" or "json", representing the output format of the model information.
%%}

  \item{timeout}{
%%     ~~Describe \code{timeout} here~~
an integer, representing the number of seconds to wait for the webservice to respond.
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
The details of the data model for the various mines are available at the websites obtained by running the following command:
paste(listMines()$URL, "/tree.do", sep="")
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
a multi-level list, representating the data model for the mine.  The first-level is a list of the InterMine objects (e.g., Gene, Exon).  Each second-level list, corresponding to an InterMine object, contains three data.frame objects: attributes, references and collections.  Each attribute is a property of the InterMine object.  Each reference or collection is itself an InterMine object, acting as a member object of the InterMine object. 
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
im <- initInterMine("humanmine.org/humanmine")

model <- getModel(im)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
