\name{NNWarehouse-class}
\Rdversion{1.1}
\docType{class}
\alias{NNWarehouse-class}
\alias{as.legal.name,NNWarehouse-method}
\alias{ClearWarehouse,NNWarehouse-method}
\alias{is.legal.name,NNWarehouse-method}
\alias{is.PnodeWarehouse,NNWarehouse-method}
\alias{is.valid,NNWarehouse-method}
\alias{WarehouseCopy,NNWarehouse,NeticaNode-method}
\alias{WarehouseData,NNWarehouse-method}
\alias{WarehouseFetch,NNWarehouse-method}
\alias{WarehouseFree,NNWarehouse-method}
\alias{WarehouseInventory,NNWarehouse-method}
\alias{WarehouseMake,NNWarehouse-method}
\alias{WarehouseSupply,NNWarehouse-method}
\alias{WarehouseManifest,NNWarehouse-method}
\alias{WarehouseManifest<-,NNWarehouse,data.frame-method}
\alias{WarehouseSave,NNWarehouse,ANY-method}

\title{Class \code{"NNWarehouse"}}
\description{

This is a container for node objects, which are instances of the
\code{\link[Peanut]{Pnode}} class.  If a requested node is not
already built, it can be built from the description found in the
warehouse.  In implements the \code{\link[Peanut]{Warehouse}} protocol.

}
\section{Objects from the Class}{

Objects can be using the constructor \code{\link{NNWarehouse}}.

  This class is a subclass of \code{PnodeWarehouse} in the
  \code{\link{Peanut-package}}.  

This is a reference object and typically there is only one instance per
project. 

}
\section{Slots}{
  \describe{
    \item{\code{manifest}:}{A data frame that gives details of how to
      build the nodes.}
    \item{\code{session}:}{Object of class
      \code{\link[RNetica]{NeticaSession}}, which is a pointer back to
      the Netica user space.}
    \item{\code{key}:}{A character vector of length two, which gives the
      name of the fields in the manifest which which identify the network
      and variable names.}
    \item{\code{prefix}:}{Object of class \code{"character"} which is
      used as a prefix if the name needs to be mangled to fit Netica
      \link{IDname} conventions. }
  }
}
\section{Extends}{
Class \code{"\linkS4class{PnodeWarehouse}"}, directly.
}
\section{Methods}{

  For all of these methods, the \code{name} argument is expected to be a
  vector of length 2 with the first component specifying the network and
  the second the node.

  \describe{

    \item{\link{WarehouseSupply}}{\code{signature(warehouse =
        "NNWarehouse", name = "character")}.  In this case the name is
        expected to be a vector of length 2 with the first component
        identifying the network and the second the node within the
        network.  This finds a node with the appropriate name in the
        referenced network.  If one does not exist,
	it is created with the properities specified in the manifest.} 

    \item{WarehouseFetch}{\code{signature(warehouse = "NNWarehouse",
      name="character")}:   Fetches the node if it already exists, or
      returns \code{NULL} if it does not. }

    \item{WarehouseMake}{\code{signature(warehouse = "NNWarehouse")}:
      Makes a new node, calling \code{\link{MakePnode.NeticaNode}}. }


    \item{as.legal.name}{\code{signature(warehouse = "NNWarehouse")}:
      If necessary, mangles a node name to follow the Netica
      \link[RNetica]{IDname} conventions. }
    \item{ClearWarehouse}{\code{signature(warehouse = "NNWarehouse")}:
      Removes prebuilt objects from the warehouse. }
    \item{is.legal.name}{\code{signature(warehouse = "NNWarehouse")}:
      Checks to see if a node name follows the Netica
      \link[RNetica]{IDname} conventions. }
    \item{is.PnodeWarehouse}{\code{signature(obj = "NNWarehouse")}:
      Returns true. }
    \item{is.valid}{\code{signature(warehouse = "NNWarehouse")}: Checks
      an object to see if it is a valid Netica Node. }
    \item{WarehouseCopy}{\code{signature(warehouse = "NNWarehouse", obj
	= "NeticaNode")}: Makes a copy of a node. }
    \item{WarehouseData}{\code{signature(warehouse = "NNWarehouse")}:
      Returns the hunk of manifest for a single node. }
    \item{WarehouseFree}{\code{signature(warehouse = "NNWarehouse")}:
      Deletes the node. }
    \item{WarehouseInventory}{\code{signature(warehouse =
	"NNWarehouse")}: Returns a list of all nodes which have already
      been built. }
    \item{WarehouseManifest}{\code{signature(warehouse =
	"NNWarehouse")}: Returns the current warehous manifest }
    \item{WarehouseManifest<-}{\code{signature(warehouse =
	"NNWarehouse", value = "data.frame")}: sets the manifest }
    \item{WarehouseSave}{\code{signature(warehouse = "NNWarehouse", obj
	= "ANY")}: Does nothing.  Saving is done at the netowrk level. }
	 }
}
\details{

  The \code{NNWarehouse} generally works with a paired
  \code{\link{BNWarehouse}} which supplies the network.  It assumes that
  the referenced network already exists or has been loaded from a file.  
  If the node already exists in the network, it simply returns it.  If
  not, it creates it using the metadata in the manifest.

  The manifest is an object of type \code{\link[base]{data.frame}} where
  the columns have the values show below.  The key is the
  pair of columns (\dQuote{Model}, \dQuote{NodeName}), with each pair
  identifying a set of rows correpsonding to the possible states of the
  node.  The \var{name} argument to
  \code{WarehouseData} should be a character vector of length 2 with the
  first component corresonding to the network name and the second to the
  node name; it will return a \code{data.frame} with multiple rows.

  Some of the fields of the  manifest data apply to the whole
  node.  In these fields, the value in the first row is used and the
  rest are ignored.

  \describe{
    \item{NStates}{A integer giving the number of states for a discrete
    variable or the discritzation of a continuous one.  The number of
    rows of the manifest data for this node should match this.}
    \item{Continuous}{A logical value telling whether or not the node
      should be regarded as continuous.}
    \item{NodeTitle}{This is a longer unconstrained name for the node.}
    \item{NodeDescription}{This is a longer string describing the node.}
    \item{NodeLabels}{This is a comma separated list of tags identifying
      sets to which the node belongs.  See
    \code{\link[Peanut]{PnodeLabels}}.} 
  }

  These fields are repeated for each of the states in the node, as they
  are different for each state.  The \dQuote{StateName} field is
  required and must be unique for each row.

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
\section{Extends}{
Class \code{"\linkS4class{PnodeWarehouse}"}, directly.

}
\references{

  The following is a Google sheet where an example node manifest can be
  found on the \code{nodes} tab.
  \url{https://docs.google.com/spreadsheets/d/1SiHQTLBNHQ-FUPnNzf9jPm9ifUG-c8f_6ljOrEcdl9M/}

}
\author{Russell Almond}
\note{

  The test for matching upper and lower bounds is perhaps too strict.
  In particular, if the upper and lower bounds mismatch by the least
  significant digit (e.g., a rounding difference) they will not match.
  This is a frequent cause of errors.

}
\seealso{
  In Peanut Package:
  \code{\link[Peanut]{Warehouse}}, \code{\link[Peanut]{WarehouseManifest}},
  \code{\link[Peanut]{BuildNodeManifest}}

  Implementation in the \code{PNetica} package:
  \code{\link{NNWarehouse}},
  \code{\link{MakePnode.NeticaNode}}

}
\examples{

sess <- NeticaSession()
startSession(sess)

## BNWarehouse is the PNetica Net Warehouse.
## This provides an example network manifest.
netman1 <- read.csv(system.file("auxdata", "Mini-PP-Nets.csv",
                     package="Peanut"),
                    row.names=1, stringsAsFactors=FALSE)
Nethouse <- BNWarehouse(manifest=netman1,session=sess,key="Name")

nodeman1 <- read.csv(system.file("auxdata", "Mini-PP-Nodes.csv", 
                     package="Peanut"),
                     row.names=1,stringsAsFactors=FALSE)

Nodehouse <- NNWarehouse(manifest=nodeman1,
                         key=c("Model","NodeName"),
                         session=sess)

CM <- WarehouseSupply(Nethouse,"miniPP_CM")
WarehouseSupply(Nethouse,"PPdurAttEM")

WarehouseData(Nodehouse,c("miniPP_CM","Physics"))
WarehouseSupply(Nodehouse,c("miniPP_CM","Physics"))

WarehouseData(Nodehouse,c("PPdurAttEM","Attempts"))
WarehouseSupply(Nodehouse,c("PPdurAttEM","Attempts"))

WarehouseData(Nodehouse,c("PPdurAttEM","Duration"))
WarehouseSupply(Nodehouse,c("PPdurAttEM","Duration"))

WarehouseFree(Nethouse,"miniPP_CM")
WarehouseFree(Nethouse,"PPdurAttEM")
stopSession(sess)

}
\keyword{classes}
