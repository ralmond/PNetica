\name{BNWarehouse}
\alias{BNWarehouse}
\title{Constructor for the \code{BNWarehosue} class.}
\description{

  This is the constructor for the \code{\linkS4class{BNWarehouse}}
  class.  This produces \code{\link[RNetica]{NeticaBN}} objects, which
  are instances of the \code{\link[Peanut]{Pnet}} abstract class.
  
}
\usage{
BNWarehouse(manifest = data.frame(), session = getDefaultSession(),
            address = ".", key = c("Name"), prefix = "S")
}
\arguments{
  \item{manifest}{A data frame containing instructions for building the
    nets.  See \code{\link[Peanut]{BuildNetManifest}}.}
  \item{session}{A link to a \code{\link[RNetica]{NeticaSession}} object
    for managing the nets.}
  \item{address}{A character scalar giving the path in which the
    \dQuote{.dne} files containing the networks are stored.}
  \item{key}{A character scalar giving the name of the column in the
    manifest which contains the network name.}
  \item{prefix}{A character scaler used in front of numeric names to
    make legal Netica names. (See \code{\link[RNetica]{as.IDname}}). }
}
\value{

  An object of class \code{\linkS4class{BNWarehouse}}.
}
\author{Russell Almond}
\seealso{
  
  \code{\link[Peanut]{Warehouse}} for the general warehouse protocol.
}
\examples{

sess <- NeticaSession()
startSession(sess)

### This tests the manifest and factory protocols.

nodeman1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
                           "Mini-PP-Nodes.csv", sep=.Platform$file.sep),
                     row.names=1,stringsAsFactors=FALSE)

netman1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
                          "Mini-PP-Nets.csv", sep=.Platform$file.sep),
                    row.names=1, stringsAsFactors=FALSE)


### Test Net building
Nethouse <- BNWarehouse(manifest=netman1,session=sess,key="Name")
stopifnot(is.PnetWarehouse(Nethouse))

setwd(paste(library(help="PNetica")$path, "testnets",sep=.Platform$file.sep))
CM <- WarehouseSupply(Nethouse,"miniPP_CM")
stopifnot(is.null(WarehouseFetch(Nethouse,"PPcompEM")))
EM1 <- WarehouseMake(Nethouse,"PPcompEM")

EMs <- lapply(c("PPcompEM","PPconjEM", "PPtwostepEM", "PPdurAttEM"),
              function(nm) WarehouseSupply(Nethouse,nm))


}
\keyword{ manip }
\keyword{ graph }
