\name{runQuery}
\alias{runQuery}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
Run a query
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
Returns results from a query against data held inside the mine. These queries are similar to SQL queries, in that they request certain defined output columns of output, filtering the results through a series of "constraints". 
}
\usage{
runQuery(im, qry, format="data.frame", timeout = 60)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{im}{
%%     ~~Describe \code{im} here~~
a list, containing the base URL and API token.
}

  \item{qry}{
%%     ~~Describe \code{query} here~~
a list or XML string, representing the query to the database. 
}
  \item{format}{
%%     ~~Describe \code{query} here~~
a string, representing the output format of the query result.  It could be one of these values: "data.frame", "sequence". 
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
If format="data.frame", the function returns a data.frame object, representing the query result.

If format="sequence", the function returns an XStringSet object, representing the DNA sequences resulted from the query.
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

queryGeneIden <- getTemplateQuery(im, "Gene_Identifiers")
queryGeneIden$view
queryGeneIden$constraints

qryRes <- runQuery(im, queryGeneIden)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
%\keyword{ ~kwd1 }
%\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
