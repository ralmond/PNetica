### Add a couble of Generic Observables so we can build
### Out the form of the two matrixes.
library(PNetica)

PP.comp <- ReadNetworks("../testnets/miniPP-CM.dne")
PP.prof <- NetworkNodesInSet(PP.comp,"Proficiency")
PP.top <- NetworkFindNode(PP.comp,"Physics")

PP.motif <- as.Pnet(PP.comp)

CompNode <- NewDiscreteNode(PP.motif,"CompensatoryObs",c("Right","Wrong"))
NodeSets(CompNode) <- c("pnodes","onodes","Observables")
NodeParents(CompNode) <- PP.prof[c("POfMom","NTL")]
PnodeQ(CompNode) <- TRUE
PnodeRules(CompNode) <- "Compensatory"
PnodeLink(CompNode) <- "partialCredit"
PnodeLnAlphas(CompNode) <- log(c(POfMom=1.1,NTL=.9))
PnodeBetas(CompNode) <- .3

ConjNode <- NewDiscreteNode(PP.motif,"ConjunctiveObs",c("Right","Wrong"))
NodeSets(ConjNode) <- c("pnodes","onodes","Observables")
NodeParents(ConjNode) <- PP.prof[c("POfMom","EnergyTransfer")]
PnodeQ(ConjNode) <- TRUE
PnodeRules(ConjNode) <- "OffsetConjunctive"
PnodeLink(ConjNode) <- "partialCredit"
PnodeLnAlphas(ConjNode) <- 0
PnodeBetas(ConjNode) <- c(POfMom=.5,EnergyTransfer=-.5)

TwoStepNode <- NewDiscreteNode(PP.motif,"TwoStepObs",c("Full","Partial","None"))
NodeSets(TwoStepNode) <- c("pnodes","onodes","Observables")
NodeParents(TwoStepNode) <- PP.prof[c("EnergyTransfer","IterativeD")]
PnodeQ(TwoStepNode) <- matrix(c(TRUE,TRUE,FALSE,TRUE),2,2,
                              dimnames=list(NULL,c("EnergyTransfer","IterativeD")))
PnodeRules(TwoStepNode) <- list("Compensatory","OffsetConjunctive")
PnodeLink(TwoStepNode) <- "partialCredit"
PnodeLnAlphas(TwoStepNode) <- list(0,0)
PnodeBetas(TwoStepNode) <- list(.5,c(EnergyTransfer=1,IterativeD=.1))

BuildAllTables(PP.motif)

WriteNetworks(PP.motif,"../testnets/PP-motif-.dne")

PP.obs <- NetworkNodesInSet(PP.motif,"Observables")

PP.omega <- Pnet2Omega(PP.motif,PP.prof)
write.csv(PP.omega,"../auxdata/miniPP-omega.csv")

PP.Q0 <- Pnet2Qmat(PP.motif,PP.obs,PP.prof)
write.csv(PP.Q0,"../auxdata/miniPP-Q.csv")



