### Add probabilities to the CM
library(PNetica)


PP.comp <- ReadNetworks("../testnets/miniPP-CM.dne")
PP.prof <- NetworkNodesInSet(PP.comp,"Proficiency")
PP.top <- NetworkFindNode(PP.comp,"Physics")

PP.comp <- as.Pnet(PP.comp)
PnetPriorWeight(PP.comp) <- 10      #Default weight



NodeSets(PP.top) <- c("Proficiency","pnode")

NodeLevels(PP.top) <- effectiveThetas(NodeNumStates(PP.top))
PnodeQ(PP.top) <- TRUE
PnodeRules(PP.top) <- "Compensatory"
PnodeLink(PP.top) <- "normalLink"
PnodeLinkScale(PP.top) <- 1
PnodeLnAlphas(PP.top) <- 1
PnodeBetas(PP.top) <- 0

### CPT for Second Level Proficiency nodes

NTL <- PP.prof$NTL
NodeLevels(NTL) <- effectiveThetas(NodeNumStates(NTL))
PnodeQ(NTL) <- TRUE
PnodeRules(NTL) <- "Compensatory"
PnodeLink(NTL) <- "normalLink"
PnodeLinkScale(NTL) <- sqrt(.2)
PnodeLnAlphas(NTL) <- log(c(Physics=sqrt(.9),EngergyTransfer=sqrt(.7)))
PnodeBetas(NTL) <- .3

pom <- PP.prof$POfMom
NodeLevels(pom) <- effectiveThetas(NodeNumStates(pom))
PnodeQ(pom) <- TRUE
PnodeRules(pom) <- "Compensatory"
PnodeLink(pom) <- "normalLink"
PnodeLinkScale(pom) <- sqrt(.2)
PnodeLnAlphas(pom) <- log(c(Physics=sqrt(.6),EngergyTransfer=sqrt(.9),
                            NTL=sqrt(.9)))
PnodeBetas(pom) <- 0

et <- PP.prof$EnergyTransfer
NodeLevels(et) <- effectiveThetas(NodeNumStates(et))
PnodeQ(et) <- TRUE
PnodeRules(et) <- "Compensatory"
PnodeLink(et) <- "normalLink"
PnodeLinkScale(et) <- sqrt(.2)
PnodeLnAlphas(et) <- log(c(Physics=sqrt(.8)))
PnodeBetas(et) <- 0


iterd <- PP.prof$IterativeD
NodeLevels(iterd) <- effectiveThetas(NodeNumStates(iterd))
PnodeQ(iterd) <- TRUE
PnodeRules(iterd) <- "Compensatory"
PnodeLink(iterd) <- "normalLink"
PnodeLinkScale(iterd) <- sqrt(.2)
PnodeLnAlphas(iterd) <- log(c(Physics=sqrt(.8)))
PnodeBetas(iterd) <- -.2


BuildAllTables(PP.comp,debug=TRUE)

WriteNetworks(PP.comp)

