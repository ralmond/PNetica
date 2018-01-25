\name{Pnet.NeticaBN}
\alias{Pnet.NeticaBN}
\alias{as.Pnet.NeticaBN}
\alias{PnetPriorWeight.NeticaBN}
\alias{PnetPriorWeight<-.NeticaBN}
\alias{PnetPnodes.NeticaBN}
\alias{PnetPnodes<-.NeticaBN}
\title{RNetica implementation of the Pnet class}
\description{

  This documentation file describes the use of a
  \code{\link[RNetica]{NeticaBN}} object as a
  \code{\link[Peanut]{Pnet}}.  See details for descriptions of the
  methods. 

}
\usage{
\method{as.Pnet}{NeticaBN}(x)
\method{PnetPriorWeight}{NeticaBN}(net)
\method{PnetPriorWeight}{NeticaBN}(net) <- value
\method{PnetPnodes}{NeticaBN}(net)
\method{PnetPnodes}{NeticaBN}(net) <- value
}
\arguments{
  \item{x}{A \code{\link[RNetica]{NeticaBN}} object to be converted to a
    \code{Pnet} object.}
  \item{net}{A \code{\link[RNetica]{NeticaBN}} object to be manipulated
    (should also be a \code{Pnet}, but this is not checked.}
  \item{value}{In the case of \code{\link[Peanut]{PnetPriorWeight}(net)}
    a numeric scalar giving the default weight for the prior.  In the
    case of \code{\link[Peanut]{PnetPnodes}(net)} a list of
    \code{\link[RNetica]{NeticaNode}} objects belonging to \code{net}.}

}
\details{

  The \code{\link[Peanut]{Pnet}} object model is added to the
  \code{\link[RNetica]{NeticaNode}} class using two approaches.  First,
  the \code{\link[Peanut]{PnetPriorWeight}} method uses the
  \code{\link[RNetica]{NetworkUserObj}} to serialize the prior weights
  and store them in one of the network's user fields
  (\dQuote{priorWeight}).  Second the \code{\link[Peanut]{PnetPnodes}}
  method uses node sets (\code{\link[RNetica]{NetworkNodesInSet}}) to
  mark the \code{\link{Pnode}}s in the graph (the node set is called
  \dQuote{pnodes}).

  In addition to the \dQuote{pnodes} node set, the \code{PNetica}
  implementation of \code{Pnet} requires an additional node set called
  \dQuote{onodes}.  These correspond to the nodes present in the
  \code{cases} argument to \code{\link[Peanut]{GEMfit}} and related
  methods.  

  The \code{as.Pnet.NeticaBN} method merely adds \dQuote{Pnet} to
  \code{class(net)}.  The default method of \code{\link[Peanut]{Pnet}}
  calls \code{as.Pnet} and also sets default values for the prior weight
  and pnodes fields.  This is the recommended approach for creating new
  \code{Pnet} objects.

  The user fields and node sets are saved and restored when a Netica
  network is saved to a file.  (This is true for the user fields in the
  \code{\link{Pnode}} objects as well.)  Calling \code{as.Pnet} on the
  newly restored network should correct the class field without
  overwriting the restored fields.  (Generally,
  \code{\link[PNetica:as.Pnode.NeticaNode]{as.Pnode}} should be called
  on all of the \code{\link{Pnode}}s as well.)
  

}
\value{

  The method \code{as.Pnet.NeticaBN} returns an object of class
  \code{c("Pnet", "NeticaBN")}.  The descriptions of the returns for the
  other methods can be found in the description of their generic
  functions.
  
}
\references{

  Almond, R. G. (2015) An IRT-based Parameterization for Conditional
  Probability Tables.  Paper presented at the 2015 Bayesian Application 
  Workshop at the Uncertainty in Artificial Intelligence Conference.

}
\author{Russell Almond}
\seealso{
  \code{\link[Peanut]{Pnet}}, \code{\link[RNetica]{NeticaBN}},
  \code{\link[Peanut]{Pnode}},  \code{\link{Pnode.NeticaNode}},
  \code{\link[Peanut]{PnetPriorWeight}},
  \code{\link[Peanut]{PnetPnodes}},
  \code{\link[RNetica]{NetworkUserObj}},
  \code{\link[RNetica]{NetworkNodesInSet}}
  
}
\examples{
sess <- NeticaSession()
startSession(sess)
#########################################
## Create network structure using RNetica calls
IRT10.2PL <- CreateNetwork("IRT10_2PL", session=sess)

theta <- NewDiscreteNode(IRT10.2PL,"theta",
                         c("VH","High","Mid","Low","VL"))
NodeLevels(theta) <- effectiveThetas(NodeNumStates(theta))
NodeProbs(theta) <- rep(1/NodeNumStates(theta),NodeNumStates(theta))

J <- 10 ## Number of items
items <- NewDiscreteNode(IRT10.2PL,paste("item",1:J,sep=""),
                         c("Correct","Incorrect"))
for (j in 1:J) {
  NodeParents(items[[j]]) <- list(theta)
  NodeLevels(items[[j]]) <- c(1,0)
  NodeSets(items[[j]]) <- c("observables")
}

## Convert into a Pnet
IRT10.2PL <- Pnet(IRT10.2PL,priorWeight=10,pnodes=items)

## Convert nodes to Pnodes
for (j in 1:J) {
  items[[j]] <- Pnode(items[[j]])
}

DeleteNetwork(IRT10.2PL)

####################################
## Restore a network from a file.
irt10.base <- ReadNetworks(file.path(library(help="PNetica")$path,
                           "testnets","IRT10.2PL.base.dne"), session=sess)
irt10.base <- as.Pnet(irt10.base)  ## Flag as Pnet, fields already set.
irt10.theta <- NetworkFindNode(irt10.base,"theta")
irt10.items <- PnetPnodes(irt10.base)
## Flag items as Pnodes
for (i in 1:length(irt10.items)) {
  irt10.items[[i]] <- as.Pnode(irt10.items[[i]])
}

## Need to set onodes field by hand, using RNetica functions
casepath <- paste(library(help="PNetica")$path,
                           "testdat","IRT10.2PL.200.items.cas",
                           sep=.Platform$file.sep)
## Record which nodes in the casefile we should pay attention to
NetworkNodesInSet(irt10.base,"onodes") <-
   NetworkNodesInSet(irt10.base,"observables")



DeleteNetwork(irt10.base)
stopSession(sess)

}
\keyword{ graphs }
\keyword{ manip }
