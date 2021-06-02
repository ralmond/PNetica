\name{NNWarehouse}
\alias{NNWarehouse}
\title{Constructor for the \code{NNWarehosue} class.}
\description{

  This is the constructor for the \code{\linkS4class{NNWarehouse}}
  class.  This produces \code{\link[RNetica]{NeticaNode}} objects, which
  are instances of the \code{\link[Peanut]{Pnode}} abstract class.
  
}
\usage{
NNWarehouse(manifest = data.frame(), session = getDefaultSession(),
            key = c("Model","NodeName"), prefix = "V")
}
\arguments{
  \item{manifest}{A data frame containing instructions for building the
    nodes.  See \code{\link[Peanut]{BuildNodeManifest}}.}
  \item{session}{A link to a \code{\link[RNetica]{NeticaSession}} object
    for managing the nets.}
  \item{key}{A character vector giving the name of the column in the
    manifest which contains the network name and the node name.}
  \item{prefix}{A character scaler used in front of numeric names to
    make legal Netica names. (See \code{\link[RNetica]{as.IDname}}). }
}
\details{

  Each network defines its own namespace for nodes, so the key to the
  node manifest is a pair (\emph{Model},\emph{NodeName}) where
  \emph{Model} is the name of the net and \code{NodeName} is the name of
  the node.  

}
\value{

  An object of class \code{\linkS4class{NNWarehouse}}.

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

### Test Node Building with already loaded nets

Nodehouse <- NNWarehouse(manifest=nodeman1,
                         key=c("Model","NodeName"),
                         session=sess)
stopifnot(is.PnodeWarehouse(Nodehouse))

phyd <- WarehouseData(Nodehouse,c("miniPP_CM","Physics"))

p3 <- MakePnode.NeticaNode(CM,"Physics",phyd)

phys <- WarehouseSupply(Nodehouse,c("miniPP_CM","Physics"))
stopifnot(p3==phys)

for (n in 1:nrow(nodeman1)) {
  name <- as.character(nodeman1[n,c("Model","NodeName")])
  if (is.null(WarehouseFetch(Nodehouse,name))) {
    cat("Building Node ",paste(name,collapse="::"),"\n")
    WarehouseSupply(Nodehouse,name)
  }
}

WarehouseFree(Nethouse,PnetName(EM1))
stopifnot(!is.valid(Nethouse,EM1))

}
\keyword{ manip }
\keyword{ graph }
