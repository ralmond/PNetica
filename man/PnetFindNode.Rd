\name{PnetFindNode}
\alias{PnetFindNode,NeticaBN-method}
\alias{PnetPnodes,NeticaBN-method}
\alias{PnetPnodes<-,NeticaBN-method}
\title{Finds nodes in a Netica Pnet. }
\description{

  The function \code{\link[Peanut]{PnetFindNode}} finds a node in a
  \code{\linkS4class{Pnet}} with the given name.  If no node with the
  specified name found, it will return \code{NULL}.

  The function \code{\link[Peanut]{PnetPnodes}} returns nodes which have
  been marked as pnodes, that is nodes that have \dQuote{pnodes} in their
  \code{\link[Peanut]{PnodeLabels}}.  

}
\usage{
\S4method{PnetFindNode}{NeticaBN}(net, name)
\S4method{PnetPnodes}{NeticaBN}(net)
\S4method{PnetPnodes}{NeticaBN}(net) <- value
}

\arguments{
  \item{net}{
    The \code{Pnet} to search.}

  \item{name}{
    A character vector giving the name or names of the desired nodes.
    Names must follow the \code{\link{IDname}} protocol.}

  \item{value}{A list of \code{\linkS4class{NeticaNode}} objects in the
    network to be marked as Pnodes.}

}
\details{
  Although each \code{\link[Peanut]{Pnode}} belongs to a single network, a
  network contains many nodes.  Within a network, a node is uniquely
  identified by its name.  However, nodes can be renamed (see
  \code{\link[RNetica]{NodeName}()}).  

  A \code{\linkS4class{NeticaNode}} is also a \code{\linkS4class{Pnode}}
  if it has the label (node set) \dQuote{pnodes}.  

  The function \code{PnetPnodes()} returns all the Pnodes in the
  network, however, the order of the nodes in the network could be
  different in different calls to this function.

  The form \code{PnetPnodes(net)<-value} sets the list of nodes in
  \code{value} to be the set of Pnodes; removing nodes which are not in
  the \code{value} from the set of Pndoes.
  
  The Pnodes are not necesarily all of the nodes in the Netica network.
  The complete list of ndoes can be found through the
  \code{RNetica::\link[RNetica]{NetworkAllNodes}} function.   
}

\value{
  The \code{\link[Peanut]{Pnode}} object or list of \code{Pnode}
  objects corresponding to \code{names}, or a list of all node objects for
  \code{PnetPnodes()}.  In the latter case, the names will be set
  to the node names.
}
\references{
\newcommand{\nref}{\href{http://norsys.com/onLineAPIManual/functions/#1.html}{#1()}}
  \url{http://norsys.com/onLineAPIManual/index.html},
    \nref{GetNodeNamed_bn}, \nref{GetNetNodes_bn}
}
\author{
  Russell Almond
}
\note{
  \code{NeticaNode} objects do not survive the life of a
  Netica session (or by implication an R session).  So the safest way to
  "save" a \code{NeticaNode} object is to recreate it using
  \code{PnetFindNode()} after the network is reloaded.
}

\seealso{

  Generic functions:
  \code{\link[Peanut]{PnetPnodes}()}, \code{\link[Peanut]{PnetFindNode}()},

  Related functions in RNetica package:
  \code{\link[RNetica]{NetworkFindNode}},
  \code{\link[RNetica]{NetworkAllNodes}}

}
\examples{
sess <- NeticaSession()
startSession(sess)

tnet <- CreateNetwork("TestNet",sess)
nodes <- NewDiscreteNode(tnet,c("A","B","C"))

nodeA <- PnetFindNode(tnet,"A")
stopifnot (nodeA==nodes[[1]])

nodeBC <- PnetFindNode(tnet,c("B","C"))
stopifnot(nodeBC[[1]]==nodes[[2]])
stopifnot(nodeBC[[2]]==nodes[[3]])

allnodes <- PnetPnodes(tnet)
stopifnot(length(allnodes)==0)

## Need to mark nodes a Pnodes before they will be seen.
nodes <- lapply(nodes,as.Pnode)
allnodes <- PnetPnodes(tnet)
stopifnot(length(allnodes)==3)
stopifnot(any(sapply(allnodes,"==",nodeA))) ## NodeA in there somewhere.

DeleteNetwork(tnet)
}
\keyword{ interface }
\keyword{ graphs }
\keyword{ utilities }

