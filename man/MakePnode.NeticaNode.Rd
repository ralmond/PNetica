\name{MakePnode.NeticaNode}
\alias{MakePnode.NeticaNode}
\title{Makes a Pnode which is also a Netica Node}
\description{
%%  ~~ A concise (1-5 lines) description of what the function does. ~~
}
\usage{
MakePnode.NeticaNode(net, name, data)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{
%%     ~~Describe \code{net} here~~
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
function (net, name, data) 
{
    node <- PnetFindNode(net, name)
    if (is.null(node)) {
        node <- NewDiscreteNode(net, name, as.character(data$StateName))
    }
    node <- as.Pnode(node)
    if (nrow(data) != as.integer(data$Nstates[1])) 
        stop("Must be one row in data for each state.")
    if (!is.null(data$NodeTitle)) 
        PnodeTitle(node) <- as.character(data$NodeTitle[1])
    if (!is.null(data$NodeDescription)) 
        PnodeDescription(node) <- as.character(data$NodeDescription[1])
    if (!is.null(data$NodeLabels)) {
        labels <- strsplit(data$NodeLabels[1], ",")[[1]]
        PnodeLabels(node) <- as.character(labels)
    }
    PnodeStates(node) <- as.character(data$StateName)
    if (!is.null(data$StateTitle)) 
        PnodeStateTitles(node) <- as.character(data$StateTitle)
    if (!is.null(data$StateDescription)) 
        PnodeStateDescriptions(node) <- as.character(data$StateDescription)
    if (!is.null(data$StateValue) && !any(is.na(data$StateValue))) 
        PnodeStateValues(node) <- as.character(data$StateValue)
    node
  }
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }% use one of  RShowDoc("KEYWORDS")
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
