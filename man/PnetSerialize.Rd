\name{PnetSerialize}
\docType{methods}
\alias{PnetSerialize-methods}
\alias{unserializePnet-methods}
\alias{PnetSerialize,NeticaBN-method}
\alias{unserializePnet,NeticaSession-method}
\title{Methods for (un)serializing a Netica Network}
\description{
 Methods for functions \code{\link[Peanut]{PnetSerialize}} and
 \code{\link[Peanut]{unserializePnet}}
 in package \pkg{Peanut}, which serialize
 \code{\link[RNetica]{NeticaBN}} objects.   Note that in this case, the
 factory is the \code{\link[RNetica]{NeticaSession}} object.  These
 methods assume that there is a global variable with the name of the
 session object which points to the Netica session.
}
\section{Methods}{
\describe{
\item{\code{PnetSerialize}, \code{signature(net = "NeticaBN")}}{
  Returns a vector with three components.  The \code{name} field is the
  name of the network.  The \code{data} component is a raw vector
  produced by calling \code{\link[base]{serialize}(...,NULL)} on the
  output of a \code{\link[RNetica]{WriteNetworks}} operation.  The
  \code{factory} component is the name of the
  \code{\link[RNetica]{NeticaSession}} object.  Note that the
  \code{\link[Peanut]{PnetUnserialize}} function assumes that there is a
  global variable with name given by the factory argument which contains
  an appropriate \code{NeticaSession} object for the restoration.
}
\item{\code{unserializePnet}, \code{signature(factory = "NeticaSession")}}{
  This method reverses the previous one.  In particular, it applies
  \code{\link[RNetica]{ReadNetworks}} to the serialized object.

}
}}
\examples{
## Need to create session whose name is is the same a the symbol it is
## stored in. 
MySession <- NeticaSession(SessionName="MySession")
startSession(MySession)

irt5 <- ReadNetworks(system.file("sampleNets","IRT5.dne",
                                 package="RNetica"), session=MySession)
NetworkAllNodes(irt5)
CompileNetwork(irt5) ## Ready to enter findings
NodeFinding(irt5$nodes$Item_1) <- "Right"
NodeFinding(irt5$nodes$Item_2) <- "Wrong"

## Serialize the network
irt5.ser <- PnetSerialize(irt5)
stopifnot (irt5.ser$name=="IRT5",irt5.ser$factory=="MySession")

NodeFinding(irt5$nodes$Item_3) <- "Right"


## now revert by unserializing.
irt5 <- PnetUnserialize(irt5.ser)
NetworkAllNodes(irt5)
stopifnot(NodeFinding(irt5$nodes$Item_1)=="Right",
          NodeFinding(irt5$nodes$Item_2)=="Wrong",
          NodeFinding(irt5$nodes$Item_3)=="@NO FINDING")

DeleteNetwork(irt5)
stopSession(MySession)          


}
\keyword{methods}
\keyword{graphs}
\keyword{IO}
