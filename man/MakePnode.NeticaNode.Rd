\name{MakePnode.NeticaNode}
\alias{MakePnode.NeticaNode}
\title{Makes a Pnode which is also a Netica Node}
\description{

  This does the actual work of making a node from a warehose manifest.
  It is typically called from \code{\link[Peanut]{WarehouseMake}}. 
}
\usage{
MakePnode.NeticaNode(net, name, data)
}
\arguments{
  \item{net}{A \code{\link[RNetica]{NeticaBN}} object in which the node
  will be created.
}
  \item{name}{The name of the node.  Ideally, this should follow the
  Netica \link[RNetica]{IDname} rules.}
  \item{data}{A \code{data.frame} with one for each state of contains
  data and meta-data about the node and states  (See details).}
}
\details{

  This is a key piece of the \code{\link[Peanut]{Warehouse}}
  infrastructure.  If a node of the designated name does not exist, it
  will be created.  If it does exist, the metadata fields of the node
  will be adjusted to match the fields in the \code{data} object.

  Some of the fields of the  \code{data} object apply to the whole
  node.  In these fields, the value in the first row is used and the
  rest are ignored.

  \describe{
    \item{NStates}{A integer giving the number of states for a discrete
    variable or the discritzation of a continuous one.  The number of
    rows of the data frame should match this.}
    \item{Continuous}{A logical value telling whether or not the node
      should be regarded as continuous.}
    \item{NodeTitle}{This is a longer unconstrained name for the node.}
    \item{NodeDescription}{This is a longer string describing the node.}
    \item{NodeLabels}{This is a comma separated list of tags identifying
      sets to which the node belongs.  See
    \code{\link[Peanut]{PnodeLabels}}.} 
  }

  These fields are repeated for each of the states in the node, as they
  are different for each state.

  \describe{
    \item{StateName}{The name of the state, this should follow the
    Netica \link[RNetica]{IDname} conventions.}
    \item{StateTitle}{This is a longer unconstrained name for the state.}
    \item{StateDescription}{This is a longer string describing the
      state.}
  }

  Additionally, the following field is used only for discrete nodes:
  \describe{
    \item{StateValue}{This is a numeric value assigned to the state.
    This value is used when calculating the node expected value.}
  }
  The StateValue plays two important roles.  First, when used with the
  \code{\link[Peanut]{PnodeEAP}} and \code{\link[Peanut]{PnodeSD}}
  functions, it is the value assigned to the node.  Second, when
  constructing CPTs using the DiBello framework, it is used at the
  effective thetas.  See \code{\link[Peanut]{PnodeParentTvals}} and
  \code{\link[Peanut]{PnodeStateValues}} 


  Continuous nodes in Netica are handled by breaking the interval up
  into pieces.  This is the function
  \code{\link[Peanut]{PnodeStateBounds}}.  Note that the bounds should
  be either monotonically increasing or decreasing and that the lower
  bound for one category should match lower bound for the next to within
  a tolerance of .002.  The values \code{Inf} and \code{-Inf} can be
  used where appropriate.

  \describe{
    \item{LowerBound}{This is a numeric value giving the lower bound for
    the range for the discritization of the node.}
    \item{UpperBound}{This is a numeric value giving the upper bound for
    the range for the}
  }

  
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

Note that the truename convention is used for node names, but not for
state names, which are restricted to Netica conventions.

}
\value{

  An object of class \code{\link[RNetica]{NeticaNode}} which is also in the
  \code{\link[Peanut]{Pnode}} abtract class.

}
\author{Russell Almond}
\seealso{
  RNetica Package:
  \code{\link[RNetica]{NeticaNode}}, 
  \code{\link[RNetica]{NewContinuousNode}},
  \code{\link[RNetica]{NewDiscreteNode}},
  \code{\link[RNetica]{IDname}}

  Peanut Package:
  \code{\link[Peanut]{Warehouse}}, \code{\link[Peanut]{WarehouseMake}}
  
  PNetica Pacakge
  \code{\link[Peanut]{PnodeWarehouse}}

}
\examples{
sess <- NeticaSession()
startSession(sess)

### This tests the manifest and factory protocols.

netman1 <- read.csv(system.file("auxdata", "Mini-PP-Nets.csv", 
                                 package="Peanut"),
                    row.names=1, stringsAsFactors=FALSE)
## Build the first network (proficiency model)
miniPP <- MakePnet.NeticaBN(sess,"miniPP",netman1[1,,drop=FALSE])

nodeman1 <- read.csv(system.file("auxdata", "Mini-PP-Nodes.csv", 
                                 package="Peanut"),
                     row.names=1,stringsAsFactors=FALSE)

## Discrete Example
phys.dat <- nodeman1[nodeman1$NodeName=="Physics",]

Physics <- MakePnode.NeticaNode(miniPP,"Physics",phys.dat)

## Continuous Example
dur.dat <- nodeman1[nodeman1$NodeName=="Duration",]

Duration <- MakePnode.NeticaNode(miniPP,"Duration",dur.dat)


DeleteNetwork(miniPP)
stopSession(sess)

}
\keyword{ manip }
\keyword{ graphs }
