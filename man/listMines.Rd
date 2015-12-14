\name{listMines}
\alias{listMines}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
List the available InterMine-powered databases
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
InterMine-powered databases such as FlyMine, modENCODE, RatMine, YeastMine, MetabolicMine and TargetMine are integrated databases of genomic, expression and protein data for various organisms.  The function listMines() lists the current available databases.
}
\usage{
listMines()
}
%- maybe also 'usage' for other objects documented here.
%\details{
%%  ~~ If necessary, more details than the description above ~~
%}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
A data frame object containing two columns:
\item{URL}{the base URL of the webservice of the database}
\item{name}{the name of the database}
}
\references{
%% ~put references to the literature/web site here ~
http://intermine.org
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
listMines()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
