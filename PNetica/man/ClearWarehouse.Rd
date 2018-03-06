\name{ClearWarehouse}
\alias{ClearWarehouse}
\title{Removes entries from a Warehouse}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
ClearWarehouse(warehouse)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{warehouse}{
%%     ~~Describe \code{warehouse} here~~
}
}
\details{
%%  ~~ If necessary, more details than the description above ~~
}
\value{
%%  ~Describe the value returned
%%  If it is a LIST, use
%%  \item{comp1 }{Description of 'comp1'}
%%  \item{comp2 }{Description of 'comp2'}
%% ...
}
\references{
%% ~put references to the literature/web site here ~
}
\author{
%%  ~~who you are~~
}
\note{
%%  ~~further notes~~
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
%% ~~objects to See Also as \code{\link{help}}, ~~~
}
\examples{
##---- Should be DIRECTLY executable !! ----
##-- ==>  Define data, use random,
##--	or do  help(data=index)  for the standard data sets.

## The function is currently defined as
structure(function (warehouse) 
standardGeneric("ClearWarehouse"), generic = structure("ClearWarehouse", package = "Peanut"), package = "Peanut", group = list(), valueClass = character(0), signature = "warehouse", default = structure(function (warehouse) 
UseMethod("ClearWarehouse"), target = structure("ANY", class = structure("signature", package = "methods"), .Names = "warehouse", package = "methods"), defined = structure("ANY", class = structure("signature", package = "methods"), .Names = "warehouse", package = "methods"), generic = structure("ClearWarehouse", package = "Peanut"), class = structure("derivedDefaultMethod", package = "methods")), skeleton = (structure(function (warehouse) 
UseMethod("ClearWarehouse"), target = structure("ANY", class = structure("signature", package = "methods"), .Names = "warehouse", package = "methods"), defined = structure("ANY", class = structure("signature", package = "methods"), .Names = "warehouse", package = "methods"), generic = structure("ClearWarehouse", package = "Peanut"), class = structure("derivedDefaultMethod", package = "methods")))(warehouse), class = structure("standardGeneric", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
