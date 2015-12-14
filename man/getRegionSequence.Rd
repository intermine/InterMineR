\name{getRegionSequence}
\alias{getRegionSequence}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Obtain the DNA sequence in regions of the genome
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Obtain the DNA sequence in regions of the genome
}
\usage{
getRegionSequence(im, regions, organism = "H. sapiens", extension = 100, 
      isInterbase = F, timeout = 60)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{im}{
%%     ~~Describe \code{im} here~~
a list containing the base URL and API token.
}
  \item{regions}{
%%     ~~Describe \code{regions} here~~
a character vector, representing genomic regions, e.g., "X:99000000..99895088".
}

  \item{organism}{
%%     ~~Describe \code{organism} here~~
a string, representing the name of the organism.
}
  \item{extension}{
%%     ~~Describe \code{extension} here~~
an integer, representing how far, in base-pairs, to extend the regions on each side.
}
  \item{isInterbase}{
%%     ~~Describe \code{isInterbase} here~~
a boolean, representing whether to treat the region as interbase co-ordinates.
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
a XstringSet object, representing the DNA sequences in the specified regions.
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

gf <- getRegionSequence(im, c("X:99000000..99895088"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
