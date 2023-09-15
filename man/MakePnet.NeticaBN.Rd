\name{MakePnet.NeticaBN}
\alias{MakePnet.NeticaBN}
\title{Creates a NeticaBN object which is also a Pnet}
\description{

  This does the actual work of making a Pnet from the manifest
  description.  It is typically called from
  \code{\link[Peanut]{WarehouseMake}}. 

}
\usage{
MakePnet.NeticaBN(sess, name, data, restoreOnly=FALSE)
}
\arguments{
  \item{sess}{The Netica session (\code{\link[RNetica]{NeticaSession}})
    object in which the net will be created.}
  \item{name}{A character scalar with the name of the network.  This
    should follow the \code{\link[RNetica]{IDname}} rules.}
  \item{data}{A list providing data and metadata about the network.  See
    details.}
  \item{restoreOnly}{A logical flag.  If true, will signal an error if
    the network file does not exist.  If false, a new empty network will
    be created.}
}
\details{

  This is a key piece of the \code{\link[Peanut]{Warehouse}}
  infrastructure.  The idea is that a network can be constructed given a
  session, a name, and a collection of metadata.  The metadata can be
  stored in a table which is the the manifest of the warehouse.

  The current system expects the following fields in the \code{data}
  argument.

  \describe{
    \item{Hub}{For a network which represents an evidence model (spoke),
    this is the name of the network to which it should be attached (the
    \emph{hub}).}
    \item{Title}{This is a longer unconstrained name for the network.}
    \item{Pathname}{This is the location in which the \code{.neta} or
    \code{.dne} file which stores the network.}
    \item{Description}{This is a longer string describing the network.}
  }
  
These correspond to fields in the \code{\link{RNetica}{NeticaBN}} object.

}
\section{Names and Truenames}{

The truename system is designed to implement the name restrictions
inherent in Netica (see \link[RNetica]{IDname}) without imposing the
same limits on the Peanut framework.  This is done by adding a
\code{Truename} field to the net object and then mangling the actual
name to follow the Netica rules using the
\code{\link[RNetica]{as.IDname}} function.

The object should be available from the warehouse via its truename, but
it is best to stick to the Netica naming conventions for networks and
nodes.


}
\note{

  There seem to be two use cases for this function (and
  \code{\link[Peanut]{WarehouseSupply}} from which it is called.  During
  model construction, calling this function should create a new blank
  network.  During scoring, it should load a prebuilt network and signal
  an error if the network is missing.  The \code{restoreOnly} flag is
  designed to distinguish between these cases.

}
\value{

  An object of class \code{\link[RNetica]{NeticaBN}} which is also in the
  \code{\link[Peanut]{Pnet}} abtract class.

}
\author{Russell Almond}
\seealso{

  RNetica Package:
  \code{\link[RNetica]{CreateNetwork}}, 
  \code{\link[RNetica]{NeticaBN}},
  \code{\link[RNetica]{IDname}}

  Peanut Package:
  \code{\link[Peanut]{Warehouse}}, \code{\link[Peanut]{WarehouseMake}}
  
  PNetica Pacakge
  \code{\link{BNWarehouse}}
}
\examples{

sess <- NeticaSession()
startSession(sess)

anet <- MakePnet.NeticaBN(sess,"Anet",
                          list(Title="A Network",Hub="",
                               Description="A Sample Network."))

DeleteNetwork(anet)

netman1 <- read.csv(system.file("auxdata", "Mini-PP-Nets.csv", 
                                 package="Peanut"),
                    row.names=1, stringsAsFactors=FALSE)
## Build the first network (proficiency model)
miniPP <- MakePnet.NeticaBN(sess,"miniPP",netman1[1,,drop=FALSE])

DeleteNetwork(miniPP)
stopSession(sess)

}
\keyword{ manip }
\keyword{ graph }
