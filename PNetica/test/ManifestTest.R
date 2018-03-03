sess <- NeticaSession()
startSession(sess)

### This tests the manifest and factory protocols.

## Test Writing Manifest
curd <- getwd()
setwd(file.path(library(help="PNetica")$path, "testnets"))

Nets <- ReadNetworks(c("miniPP-CM.dne", "PPcompEM.dne",
                       "PPconjEM.dne", "PPtwostepEM.dne"), session=sess)

for (n in 1:length(Nets)) {
  Nets[[n]] <- as.Pnet(Nets[[n]])
}


## Needed to set this the first time, but now we are good.
## hub <- PnetName(Nets[[1]])
## for (n in 2:length(Nets)) {
##   PnetHub(Nets[[n]]) <- hub
## }

netman <- BuildNetManifest(Nets)

netman1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
                          "Mini-PP-Nets.csv", sep=.Platform$file.sep),
                    row.names=1, stringsAsFactors=FALSE)

stopifnot(all.equal(netman,netman1))

## Needed to do this once but not again.
## setwd("~/Projects")
## write.csv(netman,"Mini-PP-Nets.csv",na="")

CM <- Nets[[1]]
EMs <- Nets[-1]

nodeman <- BuildNodeManifest(lapply(NetworkAllNodes(CM),as.Pnode))

for (n in 1:length(EMs)) {
  nodeman <- rbind(nodeman,
                    BuildNodeManifest(lapply(NetworkAllNodes(EMs[[n]]),
                                             as.Pnode)))
}

## Again, only needed once.
## write.csv(nodeman,"Mini-PP-Nodes.csv")

nodeman1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
                           "Mini-PP-Nodes.csv", sep=.Platform$file.sep),
                     row.names=1,stringsAsFactors=FALSE)

## Exclude node labels from this test, as they could appear in arbitrary order
stopifnot(all.equal(nodeman[,-6],nodeman1[,-6]))

nl <- strsplit(nodeman$NodeLabels,",")
nl1 <- strsplit(nodeman1$NodeLabels,",")
stopifnot(all(mapply(setequal,nl,nl1)))


## Clean up
DeleteNetwork(CM)
DeleteNetwork(EMs)


## Test Building From Manifest


## Omega1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
##                            "miniPP-omega.csv", sep=.Platform$file.sep),
##                      row.names=1,stringsAsFactors=FALSE)

## Q1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
##                            "miniPP-Q.csv", sep=.Platform$file.sep),
##                      stringsAsFactors=FALSE)

nodeman1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
                           "Mini-PP-Nodes.csv", sep=.Platform$file.sep),
                     row.names=1,stringsAsFactors=FALSE)

netman1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
                          "Mini-PP-Nets.csv", sep=.Platform$file.sep),
                    row.names=1, stringsAsFactors=FALSE)


### Test Net building
Nethouse <- BNWarehouse(manifest=netman1,session=sess,key="Name")
stopifnot(is.PnetWarehouse(Nodehouse))

setwd(paste(library(help="PNetica")$path, "testnets",sep=.Platform$file.sep))
CM <- WarehouseSupply(Nethouse,"miniPP_CM")
stopifnot(is.null(WarehouseFetch(Nethouse,"PPcompEM")))


EMs <- lapply(c("PPcompEM","PPconjEM", "PPtwostepEM"),
              function(nm) WarehouseSupply(Nethouse,nm))


netman2 <- BuildNetManifest(c(CM,EMs))

stopifnot(all.equal(netman1,netman2))

CM.nodes <- NetworkAllNodes(CM)
stopifnot(length(CM.nodes)==5L)



## setwd("~/Projects/PNetica/inst/testnets")
## Nethouse$save("miniPP_CM")
## lapply(EMs, Nethouse$save)


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

## setwd("~/Projects/PNetica/inst/testnets")
## Nethouse$save("miniPP_CM")
## lapply(EMs, Nethouse$save)

nodeman2 <- BuildNodeManifest(NetworkAllNodes(CM))

for (n in 1:length(EMs)) {
  nodeman2 <- rbind(nodeman2,
                    BuildNodeManifest(NetworkAllNodes(EMs[[n]])))
}

stopifnot(all.equal(nodeman2[,-6],nodeman1[,-6]))

## Node Labels may be in different orders.
nl2 <- strsplit(nodeman2$NodeLabels,",")
nl1 <- strsplit(nodeman1$NodeLabels,",")
stopifnot(all(mapply(setequal,nl2,nl1)))


#### Test again with unbuilt nets
DeleteNetwork(CM)
DeleteNetwork(EMs)
stopSession(sess)


sess1 <-NeticaSession()
startSession(sess1)
setwd(tempdir())

## Do it backwards, start with the Nodes and generate the nets
## On the fly.


Neth1 <- BNWarehouse(manifest=netman1,session=sess1,key="Name")

Nodeh1 <- NNWarehouse(manifest=nodeman1,session=sess1,
                         key=c("Model","NodeName"))


for (n in 1:nrow(nodeman1)) {
  name <- as.character(nodeman1[n,c("Model","NodeName")])
  if (is.null(WarehouseFetch(Neth1,name[1]))){
    cat("Building Net ",name[1],"\n")
    WarehouseSupply(Neth1,name[1])
  }
  if (is.null(WarehouseFetch(Nodeh1,name))) {
    cat("Building Node ",paste(name,collapse="::"),"\n")
    WarehouseSupply(Nodeh1,name)
  }
}

mods <- lapply(c("miniPP_CM","PPcompEM","PPconjEM", "PPtwostepEM"),
              function(nm) WarehouseSupply(Neth1,nm))

netman3 <- BuildNetManifest(mods)
stopifnot(all.equal(netman1,netman3))


nodeman3 <- BuildNodeManifest(NetworkAllNodes(mods[[1]]))

for (n in 2:length(mods)) {
  nodeman3 <- rbind(nodeman3,
                    BuildNodeManifest(NetworkAllNodes(mods[[n]])))
}

stopifnot(all.equal(nodeman3[,-6],nodeman1[,-6]))

## Node Labels may be in different orders.
nl3 <- strsplit(nodeman3$NodeLabels,",")
nl1 <- strsplit(nodeman1$NodeLabels,",")
stopifnot(all(mapply(setequal,nl3,nl1)))


setwd(curd)
stopSession(sess1)
