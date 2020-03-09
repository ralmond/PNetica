\name{BNWarehouse-class}
\Rdversion{1.1}
\docType{class}
\alias{BNWarehouse-class}
\alias{ClearWarehouse,BNWarehouse-method}
\alias{is.PnetWarehouse,BNWarehouse-method}
\alias{WarehouseData,BNWarehouse-method}
\alias{WarehouseDirectory,BNWarehouse-method}
\alias{WarehouseDirectory<-,BNWarehouse-method}
\alias{WarehouseFetch,BNWarehouse-method}
\alias{WarehouseFree,BNWarehouse-method}
\alias{WarehouseInventory,BNWarehouse-method}
\alias{WarehouseMake,BNWarehouse-method}
\alias{WarehouseManifest,BNWarehouse-method}
\alias{WarehouseManifest<-,BNWarehouse,data.frame-method}
\alias{WarehouseUnpack,BNWarehouse-method}

\title{Class \code{"BNWarehouse"}}
\description{

  A \code{\link[Peanut]{Warehouse}} (specifically a \code{PnetWarehouse})
  object which holds and builds
  \code{\link[RNetica]{NeticaBN}} objects.  In particular, its
  \code{\link[Peanut]{WarehouseManifest}} contains a network manifest (see
  \code{\link[Peanut]{BuildNetManifest}}) which contains information about how
  to either load the networks from the file system, or build them on
  demand.

}
\section{Objects from the Class}{
  Objects can be created by calls of the form \code{\link{BNWarehouse}(
    ...)}. 

  This class is a subclass of \code{PnetWarehouse} in the
  \code{\link{Peanut-package}}.  
}
\section{Methods}{

  \describe{

    \item{\link{WarehouseSupply}}{\code{signature(warehouse =
        "BNWarehouse", name = "character")}.  This finds a network
      with the appropriate name in the session.  If one does not exist,
      it is created by reading it from the pathname specified in the
      manifest.  If no file exists at the pathname, a new blank network
      with the properities specified in the manifest is created.}

    \item{\link{WarehouseFetch}}{\code{signature(warehouse =
        "BNWarehouse", name = "character")}.  This fetches the network
      with the given name from the session object, or returns
      \code{NULL} if it has not been built in Netica yet.} 

    \item{\link{WarehouseMake}}{\code{signature(warehouse =
        "BNWarehouse", name = "character")}.  This loads the network
      from a file into the Netica session, or builds the network (in the
      Netica session) using the data in the Manifest.} 

    \item{\link{WarehouseFree}}{\code{signature(warehouse =
        "BNWarehouse", name = "character")}.  This removes the network
      from the warehouse inventory.  \emph{Warning}:  This deletes the
      network.} 

    \item{\link{ClearWarehouse}}{\code{signature(warehouse =
        "BNWarehouse")}.  This removes all networks
      from the warehouse inventory. \emph{Warning}: This deletes all
      the networks.}

    \item{\link{is.PnetWarehouse}}{\code{signature(obj =
        "BNWarehouse")}.  This returns \code{TRUE}.}

    \item{\link{WarehouseManifest}}{\code{signature(warehouse =
        "BNWarehouse")}.  This returns the data frame with
      instructions on how to build networks. (see Details)}

    \item{\link{WarehouseManifest<-}}{\code{signature(warehouse =
        "BNWarehouse", value="data.frame")}.  This sets the data
      frame with instructions on how to build networks.(see Details)}

    \item{\link{WarehouseData}}{\code{signature(warehouse =
        "BNWarehouse", name="character")}.  This returns the portion
      of the data frame with instructions on how to build a particular
      network. (see Details)}

    \item{\link{WarehouseUnpack}}{\code{signature(warehouse =
        "BNWarehouse", serial="list")}.  This restores a serialized
      network, in particular, it is used for saving network state across
      sessions.  See \code{\link{PnetSerialize}} for an example.}

  }
}
\details{

  The \code{BNWarehouse} either supplies prebuilt (i.e., already in the
  Netica session) nets or builds them
  from the instructions found in the manifest.  In particular, the
  function \code{WarehouseSupply} will attempt to:
  \enumerate{
    \item{Find an existing network with \code{name} in the session.}
    \item{Try to read the network from the location given in the
      \code{Pathname} column of the manifest.}
    \item{Build a blank network, using the metadata in the manifest.}
  }

  The manifest is an object of type \code{\link[base]{data.frame}} where
  the columns have the values show below.  The key is the \dQuote{Name}
  column which should be unique for each row. The \var{name} argument to
  \code{WarehouseData} should be a character scalar corresponding to
  name, and it will return a \code{data.frame} with a single row.

  \describe{
    \item{Name}{A character value giving the name of the network.  This
      should be unique for each row and normally must conform to variable
      naming conventions.  Corresponds to the function
      \code{\link[Peanut]{PnetName}}.} 
    \item{Title}{An optional character value giving a longer human readable name
      for the netowrk. Corresponds to the function
      \code{\link[Peanut]{PnetTitle}}.} 
    \item{Hub}{If this model is incomplete without being joined to another
      network, then the name of the hub network.  Otherwise an empty
      character vector. Corresponds to the function
      \code{\link[Peanut]{PnetHub}}.} 
    \item{Pathname}{The location of the file from which the network should
      be read or to which it should be written.  Corresponds to the function
      \code{\link[Peanut]{PnetPathname}}.} 
    \item{Description}{An optional character value documenting the purpose
      of the network. Corresponds to the function
      \code{\link[Peanut]{PnetDescription}}.} 
  }
  
  The function \code{\link[Pnet]{BuildNetManifest}} will build a manifest for
  an existing collection of networks.
  
}
\section{Slots}{
  \describe{
    \item{\code{manifest}:}{A \code{data.frame} which consists of the
      manifest.  (see details).}
    \item{\code{session}:}{Object of class
      \code{\link[RNetica]{NeticaSession}}.  This is the session in
      which the nets are created.}
    \item{\code{address}:}{Object of class \code{"character"} which
      gives the path to the directory in which written descriptions of
      the nets are stored. }
    \item{\code{key}:}{Object of class \code{"character"} giving the
      name of the column which has the key for the manifest.  This is
      usually \code{"Name"}. }
    \item{\code{prefix}:}{Object of class \code{"character"} giving a
      short string to insert in front of numeric names to make legal
      Netica names (see \code{\link[RNetica]{as.IDname}}).}
  }
}
\section{Extends}{
Class \code{"\linkS4class{PnetWarehouse}"}, directly.

}
\author{Russell Almond}
\note{

  The \code{BNWarehouse} implementatation contains an
  embedded \code{\link[RNetica]{NeticaSession}} object.  When
  \code{WarehouseSupply} is called, it attempts to satisfy the demand by
  trying in order:
  \enumerate{
    \item{Search for the named network in the active networks in the
      session.}
    \item{If not found in the session, it will attempt to load the
      network from the \code{Pathname} field in the manifest.}
    \item{If the network is not found and there is not file at the
      target pathename, a new blank network is built and the appropriate
      fields are set from the metadata.}
  }

}
\seealso{
  In Peanut Package:
  \code{\link[Peanut]{Warehouse}}, \code{\link[Peanut]{WarehouseManifest}},
  \code{\link[Peanut]{BuildNetManifest}}

  Implementation in the \code{PNetica} package:
  \code{\link{BNWarehouse}},
  \code{\link[PNetica]{MakePnet.NeticaBN}}

}
\examples{
sess <- NeticaSession()
startSession(sess)

## BNWarehouse is the PNetica Net Warehouse.
## This provides an example network manifest.
netman1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
                          "Mini-PP-Nets.csv", sep=.Platform$file.sep),
                    row.names=1, stringsAsFactors=FALSE)
Nethouse <- BNWarehouse(manifest=netman1,session=sess,key="Name")

## is.PnetWarehouse -- tests for PnetWarehouse.
stopifnot(is.PnetWarehouse(Nethouse))

## WarehouseManifest
stopifnot(all.equal(WarehouseManifest(Nethouse),netman1))

## WarehouseData
stopifnot(all.equal(WarehouseData(Nethouse,"miniPP_CM")[-4],
   netman1["miniPP_CM",-4]),
   ## Pathname has leading address prefix instered.
   basename(WarehouseData(Nethouse,"miniPP_CM")$Pathname) ==
   basename(netman1["miniPP_CM","Pathname"]))

## WarehouseManifest<- 
netman2 <- netman1
netman2["miniPP_CM","Pathname"] <- "mini_CM.dne"
WarehouseManifest(Nethouse) <- netman2

stopifnot(all.equal(WarehouseData(Nethouse,"miniPP_CM")[,-4],
   netman2["miniPP_CM",-4]),
   basename(WarehouseData(Nethouse,"miniPP_CM")$Pathname) ==
   basename(netman2["miniPP_CM","Pathname"]))
WarehouseManifest(Nethouse) <- netman1

## Usually way to access nets is through warehouse supply
CM <- WarehouseSupply(Nethouse, "miniPP_CM")
EM <- WarehouseSupply(Nethouse, "PPcompEM")
stopifnot(is.active(CM),is.active(EM))

## WarehouseFetch -- Returns NULL if does not exist
stopifnot(is.null(WarehouseFetch(Nethouse,"PPconjEM")))

## WarehouseMake -- Make the net anew.
EM1 <- WarehouseMake(Nethouse,"PPconjEM")
EM1a <- WarehouseFetch(Nethouse,"PPconjEM")
stopifnot(PnetName(EM1)==PnetName(EM1a))

## WarehouseFree -- Deletes the Net
WarehouseFree(Nethouse,"PPconjEM")
stopifnot(!is.active(EM1))

## ClearWarehouse -- Deletes all nets
ClearWarehouse(Nethouse)
stopifnot(!is.active(EM),!is.active(CM))

stopSession(sess)

}
\keyword{classes}
\keyword{graphs}
