\name{Statistic.NeticaNode}
\Rdversion{1.1}
\docType{class}
\alias{Statistic.NeticaNode}
\alias{PnodeEAP,NeticaBN,NeticaNode-method}
\alias{PnodeEAP,NeticaBN,character-method}
\alias{PnodeSD,NeticaBN,NeticaNode-method}
\alias{PnodeSD,NeticaBN,character-method}
\alias{PnodeMargin,NeticaBN,NeticaNode-method}
\alias{PnodeMargin,NeticaBN,character-method}
\alias{PnodeMedian,NeticaBN,NeticaNode-method}
\alias{PnodeMedian,NeticaBN,character-method}
\alias{PnodeMode,NeticaBN,NeticaNode-method}
\alias{PnodeMode,NeticaBN,character-method}
\title{Statistic methods for \code{"NeticaBN"} class.}
\description{

  These are the implementation for the basic statistic calculation
  methods.  

}
\section{Methods}{

  All methods have signature \code{signature(net = "NeticaBN", node =
  "NeticaNode")} and \code{signature(net = "NeticaBN", node =
  "character")}.  The later form is more often used, and takes the name
  of the node and finds the appropriate node in the network.  

  \describe{
    \item{\link[Peanut]{PnodeEAP}}{Calculates the marginal distribution
      of the node. Statistic returns a named vector of values.}
    \item{\link[Peanut]{PnodeEAP}}{Calculates the expected value of the
      node; assumes numeric values have been set with
      \code{\link[Peanut]{PnodeStateValues}}. }
    \item{\link[Peanut]{PnodeSD}}{Calculates the standard deviation of
      the node; assumes numeric values have been set with
      \code{\link[Peanut]{PnodeStateValues}}. }
    \item{\link[Peanut]{PnodeMedian}}{Calculates the median state (state
      whose cumulative probability covers .5) of the node. Statistic
      returns the name of the state.} 
    \item{\link[Peanut]{PnodeMode}}{Calculates the modal (most likely) state 
      of the node. Statistic returns the name of the state.} 
  }

}
\references{
  Almond, R.G., Mislevy, R.J. Steinberg, L.S., Yan, D. and Willamson, D.
  M. (2015). \emph{{Bayesian} Networks in Educational Assessment}.
  Springer.  Chapter 13.
}
\author{Russell Almond}
\seealso{
  Statistics Class:
  \code{\linkS4class{Statistic}}

  Constructor function:
  \code{\link{Statistic}}

  \code{\link{calcStat}}

  These statistics will likely produce errors unless
  \code{\link{PnetCompile}} has been run first.
}
\examples{

sess <- NeticaSession()
startSession(sess)

irt10.base <- ReadNetworks(paste(library(help="PNetica")$path,
                           "testnets","IRT10.2PL.base.dne",
                           sep=.Platform$file.sep),session=sess)
irt10.base <- as.Pnet(irt10.base)  ## Flag as Pnet, fields already set.
irt10.theta <- PnetFindNode(irt10.base,"theta")
irt10.items <- PnetPnodes(irt10.base)
## Flag items as Pnodes
for (i in 1:length(irt10.items)) {
  irt10.items[[i]] <- as.Pnode(irt10.items[[i]])
  
}
## Make some statistics
marginTheta <- Statistic("PnodeMargin","theta","Pr(theta)")
meanTheta <- Statistic("PnodeEAP","theta","EAP(theta)")
sdTheta <- Statistic("PnodeSD","theta","SD(theta)")
medianTheta <- Statistic("PnodeMedian","theta","Median(theta)")
modeTheta <- Statistic("PnodeMedian","theta","Mode(theta)")


BuildAllTables(irt10.base)
PnetCompile(irt10.base) ## Netica requirement

calcStat(marginTheta,irt10.base)
calcStat(meanTheta,irt10.base)
calcStat(sdTheta,irt10.base)
calcStat(medianTheta,irt10.base)
calcStat(modeTheta,irt10.base)

DeleteNetwork(irt10.base)
stopSession(sess)

}
\keyword{ graphs }

