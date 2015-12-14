\name{newQuery}
\alias{newQuery}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Initialize a new query
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
A query needs to have at least view, constraints and constraintLogic.  The view is a vector containing the columns of the query output.  The constraint is a matrix containing the following columns: path (the path of the constraint), op (the constraint operator, one of '=', '!=', 'LOOKUP', 'ONE OF', 'NONE OF', '>', '<', '>=', '<=', 'LIKE'), value (the constraint value),  code (the name of the constraint), extraValue (optional, required for LOOKUP constraints).  The constraintLogic by default is "AND" operation, e.g., "A and B", where A and B are the codes in the constraints.
}
\usage{
newQuery(name="", view=character(), sortOrder="", longDescription="", 
      constraints=matrix(character(0), 0, 5,
      dimnames=list(NULL, c('path', 'op', 'value', 'code', 'extraValue'))),
      constraintLogic=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{name}{
%%     ~~Describe \code{name} here~~
a string, representing the name of the query.
}
  \item{view}{
%%     ~~Describe \code{view} here~~
a character vector, representing the fields to be selected from InterMine.
}
  \item{sortOrder}{
%%     ~~Describe \code{sortOrder} here~~
a string, representing the field according to which the query result is sorted and the sort order ("asc" or "desc"), following the format "FIELD ORDER".
}
  \item{longDescription}{
%%     ~~Describe \code{longDescription} here~~
a string, representing the description of the query.
}
  \item{constraints}{
%%     ~~Describe \code{constraints} here~~
a matrix of 5 columns (path, op, value, code, extraValue), with each row representing a constraint of the query.  
}
  \item{constraintLogic}{
%%     ~~Describe \code{constraintLogic} here~~
a string, representing the logical relationship between the constraints, e.g., "A or B" where "A" and "B" are the codes in the constraints. 
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
a list, representing the query.
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
nq <-  newQuery()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
