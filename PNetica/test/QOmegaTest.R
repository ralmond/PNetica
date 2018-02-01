sess <- NeticaSession()
startSession(sess)
curd <- getwd()
## Insures we are building nets from scratch
setwd(tempdir())


netman1 <- read.csv(paste(library(help="PNetica")$path, "auxdata",
                          "Mini-PP-Nets.csv", sep=.Platform$file.sep),
                    row.names=1,stringsAsFactors=FALSE)

nodeman1 <- read.csv(paste(library(help="PNetica")$path, "auxdata",
                           "Mini-PP-Nodes.csv", sep=.Platform$file.sep),
                     row.names=1,stringsAsFactors=FALSE)

omegamat <- read.csv(paste(library(help="PNetica")$path, "auxdata",
                           "miniPP-omega.csv", sep=.Platform$file.sep),
                     row.names=1,stringsAsFactors=FALSE)
class(omegamat) <- c("OmegMat",class(omegamat))

Q1 <- read.csv(paste(library(help="PNetica")$path, "auxdata",
                           "miniPP-Q.csv", sep=.Platform$file.sep),
               stringsAsFactors=FALSE)
class(Q1) <- c("Qmat",class(Q1))

Nethouse <- BNWarehouse(manifest=netman1,session=sess,key="Name")

Nodehouse <- NNWarehouse(manifest=nodeman1,
                         key=c("Model","NodeName"),
                         session=sess)


## Test building with the Omega matrix.  Start by making a blank net.
CM <- WarehouseSupply(Nethouse,"miniPP_CM")
CM1 <- Omega2Pnet(omegamat,CM,Nodehouse,override=TRUE,debug=TRUE)

Om2 <- Pnet2Omega(CM1,NetworkAllNodes(CM1))
omegamat$PriorWeight <- rep("",nrow(omegamat)) #Coverted to logical on read.
stopifnot(all.equal(Om2,omegamat))

Qmat2Pnet(Q1, Nethouse,Nodehouse,debug=TRUE)

stopSession(sess)
setwd(curd)
