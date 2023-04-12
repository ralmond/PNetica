\name{WarehouseDirectory}
\alias{WarehouseDirectory}
\alias{WarehouseDirectory<-}
\title{Gets or sets the directory associated with an BNWarehouse}
\description{

  If a network is not available, a \code{\link{BNWarehouse}} will look
  in the specified directory to find the \code{.dne} or \code{.neta}
  files associated with the Bayesian networks.

}
\usage{
WarehouseDirectory(warehouse)
WarehouseDirectory(warehouse) <- value
}
\arguments{
  \item{warehouse}{An object of type \code{\link{BNWarehouse}}.}
  \item{value}{A character scalar giving the new pathname for the net
    directory.} 

}
\value{

  A character string giving the path associated with a Warehouse.
}
\author{Russell Almond}
\seealso{
  \code{\link{BNWarehouse}}, \code{\link{MakePnet.NeticaBN}}
  
}
\examples{

sess <- NeticaSession()
startSession(sess)

netman1 <- read.csv(system.file("auxdata", "Mini-PP-Nets.csv", 
                                 package="Peanut"),
                    row.names=1, stringsAsFactors=FALSE)

Nethouse <- BNWarehouse(manifest=netman1,session=sess,key="Name")
stopifnot(WarehouseDirectory(Nethouse)==".")

## Set up to use a temporary directory (all networks will be built fresh)
td <- tempdir()
WarehouseDirectory(Nethouse) <- td
stopifnot(WarehouseDirectory(Nethouse)==td)

}
\keyword{ manip }
\keyword{ interface }

