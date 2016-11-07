##############################################################
## GEMfit ##
##############################################################
## Seyfullah's test script which as been causing problems.
library(PNetica)

binAll <- ReadNetworks(paste(library(help="PNetica")$path,
                             "testnets","binAll.dne",
                             sep=.Platform$file.sep))

#binAll <- ReadNetworks("binAll.dne")

binAll <- as.Pnet(binAll)  ## Flag as Pnet, fields already set.
bna.theta <- NetworkFindNode(binAll,c("theta0","theta1"))
bna.items <- PnetPnodes(binAll)
## Flag items as Pnodes
for (i in 1:length(bna.items)) {
  bna.items[[i]] <- as.Pnode(bna.items[[i]])
}


casepath <- paste(library(help="PNetica")$path,
                  "testdat",
                  "300twothetas10items.cas",
                  sep=.Platform$file.sep)

## Record which nodes in the casefile we should pay attention to
NetworkNodesInSet(binAll,"onodes") <-
  NetworkNodesInSet(binAll,"Observables")


BuildAllTables(binAll,debug=TRUE) #creates Conditional Probability tables
CompileNetwork(binAll) ## Netica requirement

item1 <- bna.items[[3]] # for t1
priB <- PnodeBetas(item1)
priA <- PnodeAlphas(item1)
priCPT <- NodeProbs(item1)

#  this also gives error
gemout <- GEMfit(binAll,casepath,trace=TRUE,debugNo=1,maxit=5)

postB <- PnodeBetas(item1)
postA <- PnodeAlphas(item1)
postCPT <- NodeProbs(item1)

## Posterior should be different
stopifnot(
    !all.equal(priA,postA),
    !all.equal(priB,postB),
    !all.equal(priCPT,postCPT)
)
