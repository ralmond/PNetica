##'c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_title(\"#1\")}",
##'"PNetica")\Sexpr{tools:::Rd_package_title("PNetica")}
##'
##'c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_description(\"#1\")}",
##'"PNetica")\Sexpr{tools:::Rd_package_description("PNetica")}
##'
##'
##'The DESCRIPTION file:
##'c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_DESCRIPTION(\"#1\")}",
##'"PNetica")\Sexpr{tools:::Rd_package_DESCRIPTION("PNetica")}
##'
##'The \code{\link[Peanut]{Peanut}} package provides a set of generic functions
##'for manipulation parameterized networks, in particular, for the abstract
##'\code{\link[Peanut]{Pnet}} and \code{\link[Peanut]{Pnode}} classes.  This
##'package provides concrete implementations of those classes using the built
##'in classes of \code{\link[RNetica]{RNetica}}.  In particular,
##'\code{\link{Pnet.NeticaBN}} extends \code{\link[RNetica]{NeticaBN}} and
##'\code{\link{Pnode.NeticaNode}} extends \code{\link[RNetica]{NeticaNode}}.
##'The documentation object \code{\link{Pnode.States}} documents additional
##'fields of this object.
##'
##'The properties of the \code{\link[Peanut]{Pnet}} and
##'\code{\link[Peanut]{Pnode}} objects are stored as serialized Netica user
##'fields (see \code{\link[RNetica]{NetworkUserObj}} and
##'\code{\link[RNetica]{NodeUserObj}}).  The documentation object
##'\code{\link{Pnode.Properties}} documents the methods.
##'
##'The \code{as.Pnet} (\code{as.Pnode}) method for a
##'\code{\link[RNetica]{NeticaBN}} (\code{\link[RNetica]{NeticaNode}}) merely
##'adds \dQuote{Pnet} (\dQuote{Pnode}) to \code{class(net)}
##'(\code{class(node)}).  All of the methods in the \code{PNetica} are defined
##'for either the \code{\link[RNetica]{NeticaBN}} or
##'\code{\link[RNetica]{NeticaNode}} object, so strictly speaking, adding the
##'\dQuote{Pnet} or \dQuote{Pnode} class is not necessary, but it is
##'recommended in case this is used in the future.
##'
##'@name PNetica-package
##'@aliases PNetica-package PNetica
##'@docType package
##'@section PNetica Specific Implementation Details:
##'
##'Here are some Netica specific details which may not be apparent from the
##'description of the generic functions in the \code{\link[Peanut]{Peanut}}
##'package.
##'
##'\enumerate{ \itemThe \code{cases} argument to \code{\link{calcPnetLLike}},
##'\code{\link{calcExpTables}} and \code{\link[Peanut]{GEMfit}} all expect the
##'pathname of a Netica case file (see \code{\link[RNetica]{write.CaseFile}}).
##'\itemThe methods \code{\link{calcPnetLLike}}, \code{\link{calcExpTables}},
##'and therefore \code{\link[Peanut]{GEMfit}} when called with a
##'\code{\link{Pnet}} as the first argument, expect that there exists a node
##'set (see \code{\link[RNetica]{NetworkNodesInSet}}) called \dQuote{onodes}
##'corresponding to the observable variables in the case file \code{cases}.
##'\itemThe function \code{\link[RNetica]{CompileNetwork}} needs to be called
##'before calls to \code{\link{calcPnetLLike}}, \code{\link{calcExpTables}} and
##'\code{\link[Peanut]{GEMfit}}.  \itemThe method
##'\code{\link[=Pnet.NeticaBN]{PnetPnodes}} stores its value in a nodeset
##'called \dQuote{pnodes}.  It is recommended that the accessor function be
##'used for modifying this field.  \itemThe \code{\link{PnetPriorWeight}} field
##'of the \code{\link{Pnet.NeticaBN}} object and all of the fields of the
##'\code{\link{Pnode.NeticaNode}} are stored in serialized user fields with
##'somewhat obvious names (see \code{\link[RNetica]{NetworkUserObj}} and
##'\code{\link[RNetica]{NodeUserObj}}).  These fields should not be used for
##'other purposes.  }
##'@author
##'c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_author(\"#1\")}",
##'"PNetica")\Sexpr{tools:::Rd_package_author("PNetica")}
##'
##'Maintainer:
##'c("\\Sexpr[results=rd,stage=build]{tools:::Rd_package_maintainer(\"#1\")}",
##'"PNetica")\Sexpr{tools:::Rd_package_maintainer("PNetica")}
##'@seealso
##'
##'\code{PNetica} depends on the following other packages.  \describe{
##'\item{list(list("RNetica"))}{A binding of the Netica C API into R.}
##'\item{list(list("Peanut"))}{An the generic functions for which this package
##'provides implementations.} \item{list(list("CPTtools"))}{A collection of
##'implementation independent Bayes net utilities.} }
##'@references
##'
##'Almond, R. G. (2015) An IRT-based Parameterization for Conditional
##'Probability Tables.  Paper presented at the 2015 Bayesian Application
##'Workshop at the Uncertainty in Artificial Intelligence Conference.
##'@keywords package graphs
##'@examples
##'
##'sess <- NeticaSession()
##'startSession(sess)
##'
##'## Building CPTs
##'tNet <- CreateNetwork("TestNet", session=sess)
##'
##'
##'theta1 <- NewDiscreteNode(tNet,"theta1",
##'                         c("VH","High","Mid","Low","VL"))
##'NodeLevels(theta1) <- effectiveThetas(NodeNumStates(theta1))
##'NodeProbs(theta1) <- rep(1/NodeNumStates(theta1),NodeNumStates(theta1))
##'theta2 <- NewDiscreteNode(tNet,"theta2",
##'                         c("VH","High","Mid","Low","VL"))
##'NodeLevels(theta2) <- effectiveThetas(NodeNumStates(theta2))
##'NodeProbs(theta2) <- rep(1/NodeNumStates(theta2),NodeNumStates(theta2))
##'
##'partial3 <- NewDiscreteNode(tNet,"partial3",
##'                            c("FullCredit","PartialCredit","NoCredit"))
##'NodeParents(partial3) <- list(theta1,theta2)
##'
##'partial3 <- Pnode(partial3,Q=TRUE, link="partialCredit")
##'PnodePriorWeight(partial3) <- 10
##'BuildTable(partial3)
##'
##'## Set up so that first skill only needed for first transition, second
##'## skill for second transition; adjust alphas to match
##'PnodeQ(partial3) <- matrix(c(TRUE,TRUE,
##'                             TRUE,FALSE), 2,2, byrow=TRUE)
##'PnodeLnAlphas(partial3) <- list(FullCredit=c(-.25,.25),
##'                                PartialCredit=0)
##'BuildTable(partial3)
##'partial4 <- NewDiscreteNode(tNet,"partial4",
##'                            c("Score4","Score3","Score2","Score1"))
##'NodeParents(partial4) <- list(theta1,theta2)
##'partial4 <- Pnode(partial4, link="partialCredit")
##'PnodePriorWeight(partial4) <- 10
##'
##'## Skill 1 used for first transition, Skill 2 used for second
##'## transition, both skills used for the 3rd.
##'
##'PnodeQ(partial4) <- matrix(c(TRUE,TRUE,
##'                             FALSE,TRUE,
##'                             TRUE,FALSE), 3,2, byrow=TRUE)
##'PnodeLnAlphas(partial4) <- list(Score4=c(.25,.25),
##'                                Score3=0,
##'                                Score2=-.25)
##'BuildTable(partial4)
##'
##'## Fitting Model to data
##'
##'irt10.base <- ReadNetworks(file.path(library(help="PNetica")$path,
##'                           "testnets","IRT10.2PL.base.dne"), session=sess)
##'irt10.base <- as.Pnet(irt10.base)  ## Flag as Pnet, fields already set.
##'irt10.theta <- NetworkFindNode(irt10.base,"theta")
##'irt10.items <- PnetPnodes(irt10.base)
##'## Flag items as Pnodes
##'for (i in 1:length(irt10.items)) {
##'  irt10.items[[i]] <- as.Pnode(irt10.items[[i]])
##'  
##'}
##'
##'
##'casepath <- file.path(library(help="PNetica")$path,
##'                           "testdat","IRT10.2PL.200.items.cas")
##'## Record which nodes in the casefile we should pay attention to
##'NetworkNodesInSet(irt10.base,"onodes") <-
##'   NetworkNodesInSet(irt10.base,"observables")
##'
##'
##'BuildAllTables(irt10.base)
##'CompileNetwork(irt10.base) ## Netica requirement
##'
##'item1 <- irt10.items[[1]]
##'priB <- PnodeBetas(item1)
##'priA <- PnodeAlphas(item1)
##'priCPT <- NodeProbs(item1)
##'
##'gemout <- GEMfit(irt10.base,casepath)
##'
##'
##'DeleteNetwork(irt10.base)
##'DeleteNetwork(tNet)
##'stopSession(sess)
##'
##'
NULL












