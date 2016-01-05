\name{RInterMine-package}
\alias{RInterMine-package}
\alias{RInterMine}
\docType{package}
\title{
R Interface with InterMine-powered databases
}
\description{
InterMine-powered databases such as FlyMine, modENCODE, RatMine, YeastMine, HumanMine and TargetMine are integrated databases of genomic, expression and protein data for various organisms.  Integrating data makes it possible to run sophisticated data mining queries that span domains of biological knowledge.  This R package provides interface with these databases through webservices.  It makes most from the correspondence of the data frame object in R and the table object in databases while hiding the details of data exchange through XML or JSON.
}
\details{
\tabular{ll}{
Package: \tab RInterMine\cr
Type: \tab Package\cr
Version: \tab 0.99.0\cr
Date: \tab 2016-01-05\cr
License: \tab LGPL\cr
Depends: \tab Biostrings, RCurl, XML, RJSONIO, sqldf, igraph\cr
Suggests: \tab Gviz\cr
}
%~~ An overview of how to use the package, including the most important functions ~~
}
\author{
InterMine Team

Maintainer: InterMine Team <info@intermine.org>
}
\references{
http://intermine.readthedocs.org/en/latest/web-services/
}

%\keyword{ package }
%\seealso{
%~~ Optional links to other man pages, e.g. ~~
%~~ \code{\link[<pkg>:<pkg>-package]{<pkg>}} ~~
%}
%\examples{
%#~~ simple examples of the most important functions ~~
%im <- initInterMine(mine="humanmine.org/humanmine",token="E1B3R749M918KdK4R2827b05r1L")

%getVersion(im)

%#getRelease(im)

%getModel(im=im)
%}
