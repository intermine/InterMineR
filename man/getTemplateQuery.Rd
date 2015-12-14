\name{getTemplateQuery}
\alias{getTemplateQuery}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Get the query contained in a template
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Get a template query for a mine.  A template contain a saved query with a view and constraint.  The user can modify this query to obtain the desired result.  The view is a vector containing the output columns of the query.  The constraint is a matrix containing the following columns: path (the path of the constraint), op (the constraint operator, one of '=', '!=', 'LOOKUP', 'ONE OF', 'NONE OF', '>', '<', '>=', '<=', 'LIKE'), value (the constraint value),  code (the name of the constraint), extraValue (optional, required for LOOKUP constraints). 
}
\usage{
getTemplateQuery(im, name, timeout=3)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{im}{
%%     ~~Describe \code{im} here~~
a list, containing the base URL and API token.
}
  \item{name}{
%%     ~~Describe \code{query} here~~
a string, representing the name of the pre-defined template.
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
a list, reprenting the query contained in the pre-defined template.  The list should contain at least two elements, view and constrain.
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

queryGeneIden <- getTemplateQuery(im, "Gene_Identifiers")
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
