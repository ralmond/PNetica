\name{MakePnet.NeticaBN}
\alias{MakePnet.NeticaBN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
%%  ~~function to do ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MakePnet.NeticaBN(sess, name, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{sess}{
%%     ~~Describe \code{sess} here~~
}
  \item{name}{
%%     ~~Describe \code{name} here~~
}
  \item{data}{
%%     ~~Describe \code{data} here~~
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
function (sess, name, data) 
{
    pname <- as.character(data$Pathname)
    if (!is.null(pname) && file.exists(pname)) {
        net <- as.Pnet(ReadNetworks(pname, sess))
        PnetName(net) <- name
    }
    else {
        net <- as.Pnet(CreateNetwork(name, sess))
    }
    if (!is.null(data$Hub)) 
        PnetHub(net) <- as.character(data$Hub)
    if (!is.null(data$Title)) 
        PnetTitle(net) <- as.character(data$Title)
    if (!is.null(data$Pathname)) 
        PnetPathname(net) <- as.character(data$Pathname)
    if (!is.null(data$Description)) 
        PnetDescription(net) <- as.character(data$Description)
    net
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
