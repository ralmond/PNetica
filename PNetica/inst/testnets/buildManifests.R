## This builds the Q, Omega and manifests for the manifest, Omega and
## Q matrix tests

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

CM <- Nets[[1]]
EMs <- Nets[-1]

netman <- BuildNetManifest(Nets)
write.csv(netman,paste(library(help="PNetica")$path, "auxdata",
                       "Mini-PP-Nets.csv", sep=.Platform$file.sep),
          na="")


nodeman <- BuildNodeManifest(NetworkAllNodes(Nets[[1]]))

for (n in 2:length(Nets)) {
  nodeman <- rbind(nodeman,
                   BuildNodeManifest(NetworkAllNodes(Nets[[n]])))
}

write.csv(nodeman,paste(library(help="PNetica")$path, "auxdata",
                        "Mini-PP-Nodes.csv", sep=.Platform$file.sep),
          na="")


Omega <- Pnet2Omega(CM,NetworkAllNodes(CM),debug=TRUE)


write.csv(Omega,paste(library(help="PNetica")$path, "auxdata",
                      "miniPP-omega.csv", sep=.Platform$file.sep),
          na="")

obs <-sapply(EMs,NetworkAllNodes)


Qmat <- Pnet2Qmat(obs,NetworkAllNodes(CM),debug=TRUE)


write.csv(Qmat,paste(library(help="PNetica")$path, "auxdata",
                  "miniPP-Q.csv", sep=.Platform$file.sep),
         na="")

stopSession(sess)

