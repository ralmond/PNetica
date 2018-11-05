\name{Pnode.NeticaNode}
\alias{Pnode.NeticaNode}
\alias{as.Pnode.NeticaNode}
\alias{PnodeNet.NeticaNode}
\alias{PnodePriorWeight.NeticaNode}
\alias{PnodeQ.NeticaNode}
\alias{PnodeRules.NeticaNode}
\alias{PnodeLink.NeticaNode}
\alias{PnodeLnAlphas.NeticaNode}
\alias{PnodeBetas.NeticaNode}
\alias{PnodeLinkScale.NeticaNode}
\title{RNetica implementation of the Pnode class}
\description{

  This documentation file describes the use of a
  \code{\link[RNetica]{NeticaNode}} object as a
  \code{\link[Peanut]{Pnode}}.  See details for descriptions of the
  methods. 

}
\usage{
\method{Pnode}{NeticaNode} (node, lnAlphas, betas, rules="Compensatory",
                            link="partialCredit", Q=TRUE, linkScale=NULL,
                            priorWeight=NULL) 
\method{as.Pnode}{NeticaNode}(x)
\method{PnodeNet}{NeticaNode}(node)
\method{PnodePriorWeight}{NeticaNode}(node)
\method{PnodeQ}{NeticaNode}(node)
\method{PnodeRules}{NeticaNode}(node)
\method{PnodeLink}{NeticaNode}(node)
\method{PnodeLnAlphas}{NeticaNode}(node)
\method{PnodeBetas}{NeticaNode}(node)
\method{PnodeLinkScale}{NeticaNode}(node)

}
\arguments{
  \item{x}{A \code{\link[RNetica]{NeticaNode}} object to be converted to a
    \code{Pnode} object.}
  \item{node}{A \code{\link[RNetica]{NeticaNode}} object to be manipulated
    (should also be a \code{Pnode}, but this is not checked.}
  \item{lnAlphas}{A numeric vector of list of numeric vectors giving the
    log slope parameters.  See \code{\link[Peanut]{PnodeLnAlphas}} for a
    description of this parameter.  If missing, the constructor will try
    to create a pattern of zero values appropriate to the \code{rules}
    argument and the number of parent variables.}
  \item{betas}{A numeric vector of list of numeric vectors giving the
    intercept parameters.  See \code{\link[Peanut]{PnodeBetas}} for a
    description of this parameter.  If missing, the constructor will try
    to create a pattern of zero values appropriate to the \code{rules}
    argument and the number of parent variables.}
  \item{rules}{The combination rule or a list of combination rules.
    These should either be names of functions or function objects.  See
    \code{\link[Peanut]{PnodeRules}} for a description of this argument.}
  \item{link}{The name of the link function or the link function
    itself.  See \code{\link[Peanut]{PnodeLink}} for a description of the link
    function.}
  \item{Q}{A logical matrix or the constant \code{TRUE} (indicating
    that the Q-matrix should be a matrix of \code{TRUE}s).  See
    \code{\link[Peanut]{PnodeQ}} for a description of this parameter.}
  \item{linkScale}{A numeric vector of link scale parameters or
    \code{NULL} if scale parameters are not needed for the chosen link
    function.  See \code{\link[Peanut]{PnodeLinkScale}} for a description of
    this parameter.}
  \item{priorWeight}{A numeric vector of weights given to the prior
    parameter values for each row of the conditional probability table
    when learning from data (or a scalar if all rows have equal prior
    weight).  See \code{\link[Peanut]{PnodePriorWeight}} for a description of
    this parameter.}
}
\details{

  The \code{\link[Peanut]{Pnode}} object model is added to the
  \code{\link[RNetica]{NeticaNode}} class using the
  \code{\link[RNetica]{NodeUserObj}} method to serialize the value 
  and store them in one of the node's user fields.  Note that most of
  the functions described above have setter as well as getter methods
  defined (see under the corresponding arguments for descriptions).

  The \code{as.Pnode.NeticaNode} method merely adds \dQuote{Pnode} to
  \code{class(net)}.  The \code{NeticaNode} method of
  \code{\link[Peanut]{Pnode}} calls \code{as.Pnode} and also sets
  default values for various Pnode fields.  This is the recommended
  approach for creating new \code{Pnode} objects.  Note that calling
  \code{Pnode.NeticaNode} will calculate defaults for the
  \code{\link[Peanut]{PnodeLnAlphas}} and \code{\link[Peanut]{PnodeBetas}}
  based on the current value of \code{\link[RNetica]{NodeParents}(node)},
  so this should be set before calling this function. (See examples).

  The user fields are saved and restored when a Netica
  network is saved to a file.  (This is true for the user fields in the
  \code{\link{Pnet}} objects as well.)  Calling \code{as.Pnode} on the
  appropriate nodes of the newly restored network should correct the
  class field without overwriting the restored fields.  (Generally,
  \code{\link[PNetica:as.Pnode.NeticaNode]{as.Pnet}} should be called
  on the \code{\link{Pnet}} as well.)

  Note that the \code{\link{PnodeParentTvals.NeticaNode}} method assumes
  that the parent variables have had numeric values assigned to their
  states using the \code{\link[RNetica]{NodeLevels}} function.

}
\value{

  The method \code{as.Pnode.NeticaNode} returns an object of class
  \code{c("Pnode", "NeticaNode")}.  The descriptions of the returns for the
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
  \code{\link[Peanut]{Pnode}}, \code{\link[RNetica]{NeticaNode}},
  \code{\link[Peanut]{Pnet}},  \code{\link{Pnet.NeticaBN}},
  \code{\link[Peanut]{PnodePriorWeight}},
  \code{\link[Peanut]{PnodeNet}},
  \code{\link[Peanut]{PnodeQ}},
  \code{\link[Peanut]{PnodeRules}},
  \code{\link[Peanut]{PnodeLink}},
  \code{\link[Peanut]{PnodeLnAlphas}},
  \code{\link[Peanut]{PnodeBetas}},
  \code{\link[Peanut]{PnodeLinkScale}},
  \code{\link{PnodeParentTvals.NeticaNode}},
  \code{\link[RNetica]{NetworkUserObj}},

  
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

## Convert into a Pnode
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

DeleteNetwork(irt10.base)
stopSession(sess)


}
\keyword{ graphs }
\keyword{ manip }
