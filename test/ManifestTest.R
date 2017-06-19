
### This tests the manifest and factory protocols.

## Test Writing Manifest

setwd(paste(library(help="PNetica")$path, "testnets",sep=.Platform$file.sep))

Nets <- ReadNetworks(c("miniPP-CM.dne", "PPcompEM.dne",
                       "PPconjEM.dne", "PPtwostepEM.dne"))

for (n in 1:length(Nets)) {
  Nets[[n]] <- as.Pnet(Nets[[n]])
}


## Needed to set this the first time, but now we are good.
## hub <- PnetName(Nets[[1]])
## for (n in 2:length(Nets)) {
##   PnetHub(Nets[[n]]) <- hub
## }

netman <- BuildNetManifest(Nets)



setwd("~/Projects")
write.csv(netman,"Mini-PP-Nets.csv",na="")

CM <- Nets[[1]]
EMs <- Nets[-1]

nodeman <- BuildNodeManifest(lapply(NetworkAllNodes(CM),as.Pnode))

for (n in 1:length(EMs)) {
  nodeman <- rbind(nodeman,
                    BuildNodeManifest(lapply(NetworkAllNodes(EMs[[n]]),
                                             as.Pnode)))
}

write.csv(nodeman,"Mini-PP-Nodes.csv")

## Clean up
DeleteNetwork(CM)
DeleteNetwork(EMs)


## Test Building From Manifest

netman1 <- read.csv(paste(library(help="PNetica")$path, "auxdata",
                          "Mini-PP-Nets.csv", sep=.Platform$file.sep),
                    stringsAsFactors=FALSE)

nodeman1 <- read.csv(paste(library(help="PNetica")$path, "auxdata",
                           "Mini-PP-Nodes.csv", sep=.Platform$file.sep),
                     stringsAsFactors=FALSE)


Omega1 <- read.csv(paste(library(help="PNetica")$path, "auxdata",
                           "miniPP-omega.csv", sep=.Platform$file.sep),
                     stringsAsFactors=FALSE)

Q1 <- read.csv(paste(library(help="PNetica")$path, "auxdata",
                           "miniPP-Q.csv", sep=.Platform$file.sep),
                     stringsAsFactors=FALSE)




### Test Net building
Nethouse <- PnetWarehouse$new(type="NeticaBN",manifest=netman1)

setwd(paste(library(help="PNetica")$path, "testnets",sep=.Platform$file.sep))
CM <- Nethouse$supply("miniPP_CM")


EMs <- lapply(c("PPcompEM","PPconjEM", "PPtwostepEM"),
              function(nm) Nethouse$supply(nm))
sapply(EMs,PnetTitle)

## setwd("~/Projects/PNetica/inst/testnets")
## Nethouse$save("miniPP_CM")
## lapply(EMs, Nethouse$save)


### Test Node Building with already loaded nets

Nodehouse <- PnodeWarehouse$new(type="NeticaNode",manifest=nodeman1,
                                key=c("Model","NodeName"),
                                pnetwarehouse=Nethouse)

phyd <- Nodehouse$manifestData(c("miniPP_CM","Physics"))

MakePnode.NeticaNode(CM,"Physics",phyd)

phys <- Nodehouse$supply(c("miniPP_CM","Physics"))

for (n in 1:nrow(Nodehouse$manifest)) {
  name <- as.character(nodeman1[n,c("Model","NodeName")])
  if (is.null(Nodehouse$fetch(name)))
    Nodehouse$supply(name)
}

## setwd("~/Projects/PNetica/inst/testnets")
## Nethouse$save("miniPP_CM")
## lapply(EMs, Nethouse$save)



#### Test again with unbuilt nets

Nethouse$delete("miniPP_CM")
lapply(c("PPcompEM","PPconjEM", "PPtwostepEM"),Nethouse$delete)
stopifnot(length(Nethouse$inventory)==0L)



