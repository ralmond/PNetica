\name{PnetHub}
\alias{PnetHub}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
PnetHub(net)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{
%%     ~~Describe \code{net} here~~
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
structure(function (net) 
standardGeneric("PnetHub"), generic = structure("PnetHub", package = "Peanut"), package = "Peanut", group = list(), valueClass = character(0), signature = "net", default = structure(function (net) 
UseMethod("PnetHub"), target = structure("ANY", class = structure("signature", package = "methods"), .Names = "net", package = "methods"), defined = structure("ANY", class = structure("signature", package = "methods"), .Names = "net", package = "methods"), generic = structure("PnetHub", package = "Peanut"), class = structure("derivedDefaultMethod", package = "methods")), skeleton = (structure(function (net) 
UseMethod("PnetHub"), target = structure("ANY", class = structure("signature", package = "methods"), .Names = "net", package = "methods"), defined = structure("ANY", class = structure("signature", package = "methods"), .Names = "net", package = "methods"), generic = structure("PnetHub", package = "Peanut"), class = structure("derivedDefaultMethod", package = "methods")))(net), class = structure("standardGeneric", package = "methods"))
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
