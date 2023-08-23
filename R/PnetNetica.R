### Parameterized networks.


## Parameterized networks have the following properties:

## A node set called Pnodes which contains a list of all Pnodes to
## maximize.
## A field called "priorWeight" which gives the default prior weight
## to use.

## This is a total hack, but R won't let me modify Peanut after it is
## locked and loaded.
setClassUnion("net.bridge","NeticaBN")
setIs("net.bridge","Pnet")
setMethod("as.Pnet","NeticaBN",function(x) x)
setMethod("is.Pnet","NeticaBN",function(x) TRUE)


## as.Pnet.NeticaBN <- function (x) {
##   if (!("Pnet" %in% class(x)))
##     class(x) <- c(class(x),"Pnet")
##   x
## }

setMethod("PnetPriorWeight","NeticaBN", function (net) {
  NetworkUserObj(net,"priorWeight")
})

setMethod("PnetPriorWeight<-","NeticaBN", function (net,value) {
  NetworkUserObj(net,"priorWeight") <- value
  invisible(net)
})

setMethod("PnetPnodes","NeticaBN", function (net) {
  NetworkNodesInSet(net,"pnodes")
})
setMethod("PnetPnodes<-","NeticaBN", function (net, value) {
  NetworkNodesInSet(net,"pnodes") <- value
  invisible(net)
})

## To fit PnetFactory Protocol





##'Creates a NeticaBN object which is also a Pnet
##'
##'
##'This does the actual work of making a Pnet from the manifest description.
##'It is typically called from \code{\link[Peanut]{WarehouseMake}}.
##'
##'
##'This is a key piece of the \code{\link[Peanut]{Warehouse}} infrastructure.
##'The idea is that a network can be constructed given a session, a name, and a
##'collection of metadata.  The metadata can be stored in a table which is the
##'the manifest of the warehouse.
##'
##'The current system expects the following fields in the \code{data} argument.
##'
##'\describe{ \item{Hub}{For a network which represents an evidence model
##'(spoke), this is the name of the network to which it should be attached (the
##'\emph{hub}).} \item{Title}{This is a longer unconstrained name for the
##'network.} \item{Pathname}{This is the location in which the \code{.neta} or
##'\code{.dne} file which stores the network.} \item{Description}{This is a
##'longer string describing the network.} }
##'
##'These correspond to fields in the \code{\link{RNetica}{NeticaBN}} object.
##'
##'@param sess The Netica session (\code{\link[RNetica]{NeticaSession}}) object
##'in which the net will be created.
##'@param name A character scalar with the name of the network.  This should
##'follow the \code{\link[RNetica]{IDname}} rules.
##'@param data A list providing data and metadata about the network.  See
##'details.
##'@return
##'
##'An object of class \code{\link[RNetica]{NeticaBN}} which is also in the
##'\code{\link[Peanut]{Pnet}} abtract class.
##'@section Names and Truenames:
##'
##'The truename system is designed to implement the name restrictions inherent
##'in Netica (see \link[RNetica]{IDname}) without imposing the same limits on
##'the Peanut framework.  This is done by adding a \code{Truename} field to the
##'net object and then mangling the actual name to follow the Netica rules
##'using the \code{\link[RNetica]{as.IDname}} function.
##'
##'The object should be available from the warehouse via its truename, but it
##'is best to stick to the Netica naming conventions for networks and nodes.
##'@author Russell Almond
##'@seealso
##'
##'RNetica Package: \code{\link[RNetica]{CreateNetwork}},
##'\code{\link[RNetica]{NeticaBN}}, \code{\link[RNetica]{IDname}}
##'
##'Peanut Package: \code{\link[Peanut]{Warehouse}},
##'\code{\link[Peanut]{WarehouseMake}}
##'
##'PNetica Pacakge \code{\link{BNWarehouse}}
##'@keywords manip graph
##'@examples
##'
##'
##'sess <- NeticaSession()
##'startSession(sess)
##'
##'anet <- MakePnet.NeticaBN(sess,"Anet",
##'                          list(Title="A Network",Hub="",
##'                               Description="A Sample Network."))
##'
##'DeleteNetwork(anet)
##'
##'netman1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
##'                          "Mini-PP-Nets.csv", sep=.Platform$file.sep),
##'                    row.names=1, stringsAsFactors=FALSE)
##'## Build the first network (proficiency model)
##'miniPP <- MakePnet.NeticaBN(sess,"miniPP",netman1[1,,drop=FALSE])
##'
##'DeleteNetwork(miniPP)
##'stopSession(sess)
##'
##'
##'@export MakePnet.NeticaBN
MakePnet.NeticaBN <-function (sess,name,data,restoreOnly=FALSE) {
  pname <- as.character(data$Pathname)
  ## this will convert NULL to character(0)
  net <- NULL
  if (length(pname) >0L && nchar(pname) > 0L) {
    if (file.exists(pname)) {
      net <-as.Pnet(ReadNetworks(pname,sess))
      PnetName(net) <- name
    } else if (restoreOnly) {
      stop(paste("Can't create",name,", can't find file ",pname))
    }
  }
  if (is.null(net)) {
    net <- as.Pnet(CreateNetwork(as.IDname(name),sess))
    NetworkUserField(net,"Truename") <- name
  }
  if (!is.null(data$Hub) && !is.na(data$Hub))
    PnetHub(net) <- trimws(as.character(data$Hub))
  if (!is.null(data$Title) && !is.na(data$Title))
    PnetTitle(net) <- as.character(data$Title)
  if (!is.null(data$Pathname) && !is.na(data$Pathname))
    PnetPathname(net) <- as.character(data$Pathname)
  if (!is.null(data$Description) && !is.na(data$Description))
    PnetDescription(net) <- as.character(data$Description)
  net
}

## Leave this as a no-op for now.
Free.NeticaBN <- function (obj) {invisible(NULL)}


Save.NeticaBN <- function (net,pathname) {
  if (missing(pathname) || is.null(pathname))
    pathname <- PnetPathname(net)
  WriteNetworks(net,pathname)
}

Reload.NeticaBN <- function (net,pathname) {
  if (missing(pathname) || is.null(pathname))
    pathname <- PnetPathname(net)
  DeleteNetwork(net)
  ReadNetworks(pathname)
}
Delete.NeticaBN <- function (obj) {
  if (!is.null(obj))
    DeleteNetwork(obj)
}

### Hub and spoke model.

## To make a stub, copy the node into the new net.  It will become a stub when it is
## deleted later
setMethod("PnetMakeStubNodes","NeticaBN", function (net,nodes) {
  if (!is.list(nodes)) nodes <- list(nodes)
  out <- CopyNodes(nodes,newnet=net)
  if (!is.list(out)) out <- list(out)
  out
})

## Deleting the node makes it a stub.
setMethod("PnetRemoveStubNodes","NeticaBN", function (net,nodes) {
  DeleteNodes(nodes)
})

##'Merges (or separates) two Pnets with common variables
##'
##'
##'In the hub-and-spoke Bayes net construction method, number of spoke models
##'(evidence models in educational applications) are connected to a central hub
##'model (proficiency models in educational applications).  The
##'\code{PnetAdjoin} operation combines a hub and spoke model to make a motif,
##'replacing references to hub variables in the spoke model with the actual hub
##'nodes.  The \code{PnetDetach} operation reverses this.
##'
##'
##'The hub-and-spoke model for Bayes net construction (Almond and Mislevy,
##'1999; Almond, 2017) divides a Bayes net into a central hub model and a
##'collection of spoke models.  The motivation is that the hub model represents
##'the status of a system---in educational applications, the proficiency of the
##'student---and the spoke models are related to collections of evidence that
##'can be collected about the system state.  In the educational application,
##'the spoke models correspond to a collection of observable outcomes from a
##'test item or task.  A \emph{motif} is a hub plus a collection of spoke model
##'corresponding to a single task.
##'
##'While the hub model is a complete Bayesian network, the spoke models are
##'fragments.  In particular, several hub model variables are parents of
##'variables in the spoke model.  These variables are not defined in spoke
##'model, but are rather replaced with \emph{stub nodes}, nodes which
##'reference, but do not define the spoke model.
##'
##'The \code{PnetAdjoin} operation copies the \code{\link{Pnode}}s from the
##'spoke model into the hub model, and connects the stub nodes to the nodes
##'with the same name in the spoke model.  The result is a motif consisting of
##'the hub and the spoke.  (If this operation is repeated many times it can be
##'used to build an arbitrarily complex motif.)
##'
##'The \code{PnetDetach} operation reverses the adjoin operation.  It removes
##'the nodes associated with the spoke model only, leaving the joint
##'probability distribution of the hub model (along with any evidence absorbed
##'by setting values of observable variables in the spoke) intact.
##'
##'@aliases PnetAdjoin,NeticaBN-method PnetDetach,NeticaBN-method
##'@param hub A complete \code{\link{Pnet}} to which new variables will be
##'added.
##'@param spoke An incomplete \code{\link{Pnet}} which may contain stub nodes,
##'references to nodes in the \code{hub}
##'@param motif The combined \code{\link{Pnet}} which is formed by joining a
##'hub and spoke together.
##'@return
##'
##'The function \code{PnetAdjoin} returns a list of the newly created nodes
##'corresponding to the spoke model nodes.  Note that the names may have
##'changed to avoid duplicate names.  The names of the list are the spoke node
##'names, so that any name changes can be discovered.
##'
##'In both cases, the first argument is destructively modified, for
##'\code{PnetAdjoin} the hub model becomes the motif.  For \code{PnetDetach}
##'the motif becomes the hub again.
##'@note
##'
##'Node names must be unique within a Bayes net.  If several spokes are
##'attached to a hub and those spokes have common names for observable
##'variables, then the names will need to be modified to make them unique.  The
##'function \code{PnetAdjoin} always returns the new nodes so that any name
##'changes can be noted by the calling program.
##'
##'I anticipate that there will be considerable varation in how these functions
##'are implemented depending on the underlying implementation of the Bayes net
##'package.  In particular, there is no particular need for the
##'\code{PnetDetach} function to do anything.  While removing variables
##'corresponding to an unneeded spoke model make the network smaller, they are
##'harmless as far as calculations of the posterior distribution.
##'@section Known Bugs:
##'
##'Netica version 5.04 has a bug that when nodes with no graphical information
##'(e.g., position) are absorbed in a net in which some of the nodes have
##'graphical information, it will generate an error.  This was found and fixed
##'in version 6.07 (beta) of the API.  However, the function \code{PnetDetach}
##'may generate internal Netica errors in this condition.
##'
##'Right now they are logged, but nothing is done.  Hopefully, they are
##'harmless.
##'@author Russell Almond
##'@seealso
##'
##'\code{\link{Pnet}}, \code{\link{PnetHub}}, \code{\link{Qmat2Pnet}},
##'\code{\link{PnetMakeStubNodes}}
##'@references
##'
##'Almond, R. G. & Mislevy, R. J. (1999) Graphical models and computerized
##'adaptive testing.  \emph{Applied Psychological Measurement}, 23, 223--238.
##'
##'Almond, R., Herskovits, E., Mislevy, R. J., & Steinberg, L. S. (1999).
##'Transfer of information between system and evidence models. In Artificial
##'Intelligence and Statistics 99, Proceedings (pp. 181--186). Morgan-Kaufman
##'
##'Almond, R. G. (presented 2017, August). Tabular views of Bayesian networks.
##'In John-Mark Agosta and Tomas Singlair (Chair), \emph{Bayeisan Modeling
##'Application Workshop 2017}. Symposium conducted at the meeting of
##'Association for Uncertainty in Artificial Intelligence, Sydney, Australia.
##'(International) Retrieved from \url{http://bmaw2017.azurewebsites.net/}
##'@keywords graphs manip
##'@examples
##'
##'
##'sess <- NeticaSession()
##'startSession(sess)
##'
##'PM <- ReadNetworks(file.path(library(help="PNetica")$path, "testnets",
##'     "miniPP-CM.dne"), session=sess)
##'EM1 <- ReadNetworks(file.path(library(help="PNetica")$path, "testnets",
##'      "PPcompEM.dne"), session=sess)
##'
##'Phys <- PnetFindNode(PM,"Physics")
##'
##'## Prior probability for high level node
##'PnetCompile(PM)
##'bel1 <- PnodeMargin(PM, Phys)
##'
##'## Adjoin the networks.
##'EM1.obs <- PnetAdjoin(PM,EM1)
##'PnetCompile(PM)
##'
##'## Enter a finding
##'PnodeEvidence(EM1.obs[[1]]) <- "Right"
##'## Posterior probability for high level node
##'
##'bel2 <- PnodeMargin(PM,Phys)
##'
##'PnetDetach(PM,EM1)
##'PnetCompile(PM)
##'
##'## Findings are unchanged
##'bel2a <- PnodeMargin(PM,Phys)
##'stopifnot(all.equal(bel2,bel2a,tol=1e-6))
##'
##'DeleteNetwork(list(PM,EM1))
##'stopSession(sess)
##'
##'@exportMethod
setMethod("PnetAdjoin","NeticaBN", function (hub, spoke) {
  AdjoinNetwork(hub,spoke,paste("Spoke",NetworkName(spoke),sep="_"))
})

setMethod("PnetDetach","NeticaBN", function (motif, spoke) {
    ## Bug in RN_AbsorbNodes
    spokename <- paste("Spoke",NetworkName(spoke),sep="_")
    tryCatch(
        AbsorbNodes(NetworkNodesInSet(motif,spokename)),
        error = function (e) {
            flog.error("While absorbing nodes from %s in %s, got error %s",
                       spokename,NetworkName(motif),conditionMessage(e))
            flog.info("This could be a known Netica bug in version 5.04")
        })
    motif
})




## A parameterized node has the following fields:

## rules -- the name of the structure function
## link -- the name of the link function
## lnAlphas -- a list of discrimination parameters
## betas -- a list of difficulty parameters
## linkScale -- a list of scale parameters
## priorWeight -- a numeric value or a vector of numeric values for
## each row of the CPT.   Inherits from the net if not available.



##'Properties of class \code{"NeticaNode"} as a \code{"Pnode"}
##'
##'
##'The \code{PNetica} package supplies the needed methods so that the
##'\code{RNetica::\linkS4class{NeticaNode}} object is an instance of the
##'\code{Peanut::\linkS4class{Pnode}} object.  As a Pnode is nominally
##'parameterized, the are given the special label \dQuote{pnode} to indicate
##'that this note has parametric information.  This document describes the
##'extra properties of \code{\linkS4class{Pnode}}s that are added by
##'\code{PNetica}.
##'
##'
##'@name Pnode.Properties
##'@aliases Pnode.Properties as.Pnode,NeticaNode-method
##'is.Pnode,NeticaNode-method Pnode,NeticaNode-method
##'PnodeLnAlphas,NeticaNode-method PnodeLnAlphas<-,NeticaNode-method
##'PnodeBetas,NeticaNode-method PnodeBetas<-,NeticaNode-method
##'PnodeQ,NeticaNode-method PnodeQ<-,NeticaNode-method
##'PnodeRules,NeticaNode-method PnodeRules<-,NeticaNode-method
##'PnodeLink,NeticaNode-method PnodeLink<-,NeticaNode-method
##'PnodeLinkScale,NeticaNode-method PnodeLinkScale<-,NeticaNode-method
##'PnodePostWeight,NeticaNode-method PnodePriorWeight,NeticaNode-method
##'PnodePriorWeight<-,NeticaNode-method
##'@docType class
##'@note
##'
##'The \dQuote{Pnode properies}, \code{lnAlphas}, \code{betas}, \code{Q},
##'\code{rules}, \code{link}, \code{linkScale}, and \code{priorWeight} are
##'stored in user fields (\code{\link{NodeUserObj}}) of the Netica node.  A
##'\code{\linkS4class{NeticaNode}} object which has those fields behaves as a
##'\code{\linkS4class{Pnode}} and is suitable for the use with
##'\code{\link[Peanut:Peanut-package]{Peanut}}.  The function \code{Pnode} will
##'add default values for these fields if they are not set.
##'
##'To mark a node as a \code{Pnode}, it is added to the
##'\link[RNetica:NodeSets]{node set} \dQuote{pnode}.  The \code{is.Pnode}
##'function checks for this method.
##'@section Extends:
##'
##'See \code{\linkS4class{NeticaNode}} for a description of the Netica class.
##'
##'With these methods, \code{\linkS4class{NeticaNode}} now extends
##'\code{\linkS4class{Pnode}}.
##'
##'All reference classes extend and inherit methods from
##'\code{"\linkS4class{envRefClass}"}.
##'@author Russell Almond
##'@seealso
##'
##'Other methods of this class \code{\link{Pnode.NeticaNode}},
##'\code{\link{Pnode.Properties}}.
##'
##'Base class: \code{\linkS4class{NeticaNode}}.
##'
##'Mixin class: \code{\linkS4class{Pnode}}.
##'
##'Generic functions from \code{Peanut} package:
##'
##'\code{\link[Peanut]{PnodeLnAlphas}}, \code{\link[Peanut]{PnodeBetas}},
##'\code{\link[Peanut]{PnodeQ}}, \code{\link[Peanut]{PnodeRules}},
##'\code{\link[Peanut]{PnodeLink}}, \code{\link[Peanut]{PnodeLinkScale}},
##'\code{\link[Peanut]{PnodePostWeight}},
##'\code{\link[Peanut]{PnodePriorWeight}}.
##'@keywords classes graphs
##'@examples
##'
##'sess <- NeticaSession()
##'startSession(sess)
##'curd <- getwd()
##'setwd(file.path(library(help="PNetica")$path, "testnets"))
##'
##'tNet <- CreateNetwork("TestNet",sess)
##'
##'## Alphas
##'theta1 <- NewDiscreteNode(tNet,"theta1",
##'                         c("VH","High","Mid","Low","VL"))
##'PnodeStateValues(theta1) <- effectiveThetas(PnodeNumStates(theta1))
##'PnodeProbs(theta1) <- rep(1/PnodeNumStates(theta1),PnodeNumStates(theta1))
##'theta2 <- NewDiscreteNode(tNet,"theta2",
##'                         c("VH","High","Mid","Low","VL"))
##'PnodeStateValues(theta2) <- effectiveThetas(PnodeNumStates(theta2))
##'PnodeProbs(theta2) <- rep(1/PnodeNumStates(theta1),PnodeNumStates(theta2))
##'
##'partial3 <- NewDiscreteNode(tNet,"partial3",
##'                            c("FullCredit","PartialCredit","NoCredit"))
##'PnodeParents(partial3) <- list(theta1,theta2)
##'
##'## Usual way to set rules is in constructor
##'partial3 <- Pnode(partial3,rules="Compensatory", link="partialCredit")
##'PnodePriorWeight(partial3) <- 10
##'BuildTable(partial3)
##'
##'## slopes of 1 for both transitions
##'PnodeLnAlphas(partial3) <- c(0,0)
##'BuildTable(partial3)
##'
##'## log slope 0 = slope 1
##'stopifnot(
##'   all(abs(PnodeAlphas(partial3) -1) <.0001)
##')
##'
##'## Make Skill 1 more important than Skill 2
##'PnodeLnAlphas(partial3) <- c(.25,-.25)
##'BuildTable(partial3)
##'
##'## increasing intercepts for both transitions
##'PnodeLink(partial3) <- "gradedResponse"
##'PnodeBetas(partial3) <- list(FullCredit=1,PartialCredit=0)
##'BuildTable(partial3)
##'stopifnot(
##'   all(abs(do.call("c",PnodeBetas(partial3)) -c(1,0) ) <.0001)
##')
##'
##'
##'## increasing intercepts for both transitions
##'PnodeLink(partial3) <- "partialCredit"
##'## Full Credit is still rarer than partial credit under the partial
##'## credit model
##'PnodeBetas(partial3) <- list(FullCredit=0,PartialCredit=0)
##'BuildTable(partial3)
##'stopifnot(
##'   all(abs(do.call("c",PnodeBetas(partial3)) -c(0,0) ) <.0001)
##')
##'
##'
##'## Make Skill 1 more important for the transition to ParitalCredit
##'## And Skill 2 more important for the transition to FullCredit
##'PnodeLnAlphas(partial3) <- list(FullCredit=c(-.25,.25),
##'                                PartialCredit=c(.25,-.25))
##'BuildTable(partial3)
##'
##'## Set up so that first skill only needed for first transition, second
##'## skill for second transition; Adjust alphas to match
##'PnodeQ(partial3) <- matrix(c(TRUE,TRUE,
##'                             TRUE,FALSE), 2,2, byrow=TRUE)
##'PnodeLnAlphas(partial3) <- list(FullCredit=c(-.25,.25),
##'                                PartialCredit=0)
##'BuildTable(partial3)
##'
##'## Using OffsetConjunctive rule requires single slope
##'PnodeRules(partial3) <- "OffsetConjunctive"
##'## Single slope parameter for each transition
##'PnodeLnAlphas(partial3) <- 0
##'PnodeQ(partial3) <- TRUE
##'PnodeBetas(partial3) <- c(0,1)
##'BuildTable(partial3)
##'
##'## Make Skill 1 more important for the transition to ParitalCredit
##'## And Skill 2 more important for the transition to FullCredit
##'PnodeLnAlphas(partial3) <- 0
##'PnodeBetas(partial3) <- list(FullCredit=c(-.25,.25),
##'                                PartialCredit=c(.25,-.25))
##'BuildTable(partial3)
##'
##'
##'## Separate slope parameter for each transition;  
##'## Note this will only different from the previous transition when
##'## mapDPC is called.  In the former case, it will learn a single slope
##'## parameter, in the latter, it will learn a different slope for each
##'## transition. 
##'PnodeLnAlphas(partial3) <- list(0,0)
##'BuildTable(partial3)
##'
##'## Set up so that first skill only needed for first transition, second
##'## skill for second transition; Adjust betas to match
##'PnodeQ(partial3) <- matrix(c(TRUE,TRUE,
##'                             TRUE,FALSE), 2,2, byrow=TRUE)
##'PnodeBetas(partial3) <- list(FullCredit=c(-.25,.25),
##'                                PartialCredit=0)
##'BuildTable(partial3)
##'
##'
##'## Can also do this with special parameter values
##'PnodeQ(partial3) <- TRUE
##'PnodeBetas(partial3) <- list(FullCredit=c(-.25,.25),
##'                                PartialCredit=c(0,Inf))
##'BuildTable(partial3)
##'
##'## The normal link function is the only one which takes a scale parameter
##'PnodeLink(partial3) <- "normalLink"
##'PnodeLinkScale(partial3) <- 1.0
##'PnodeLnAlphas(partial3) <- 0
##'PnodeBetas(partial3) <- c(0,1)
##'BuildTable(partial3)
##'stopifnot(
##'  all(abs(PnodeLinkScale(partial3)-1)<.0001)
##')
##'
##'DeleteNetwork(tNet)
##'
##'stopSession(sess)
##'setwd(curd)
##'
##'
NULL

setClassUnion("node.bridge","NeticaNode")
setIs("node.bridge","Pnode")

setMethod("as.Pnode","NeticaNode",function(x) {
  NodeSets(x) <- union("pnodes",NodeSets(x))
  if (is.na(NodeUserField(x,"Truename")))
    NodeUserField(x,"Truename") <- NodeName(x)
  x})
setMethod("is.Pnode","NeticaNode",function(x)
  "pnodes" %in% NodeSets(x)
  )


## as.Pnode.NeticaNode <- function (x) {
##   if (!("Pnode" %in% class(x)))
##     class(x) <- c(class(x),"Pnode")
##   x
## }

setMethod("PnodeNet","NeticaNode", function (node) {
  NodeNet(node)
})

setMethod("PnodeRules","NeticaNode", function (node) {
  NodeUserObj(node,"rules")
})

setMethod("PnodeRules<-","NeticaNode", function (node,value) {
  NodeUserObj(node,"rules") <- value
  node
})

setMethod("PnodeLink","NeticaNode", function (node) {
  NodeUserObj(node,"link")
})

setMethod("PnodeLink<-","NeticaNode", function (node,value) {
  NodeUserObj(node,"link") <- value
  node
})

setMethod("PnodeQ","NeticaNode", function (node) {
  NodeUserObj(node,"Q")
})

setMethod("PnodeQ<-","NeticaNode", function (node,value) {
  NodeUserObj(node,"Q") <- value
  node
})

setMethod("PnodeLnAlphas","NeticaNode", function (node) {
  NodeUserObj(node,"lnAlphas")
})

setMethod("PnodeLnAlphas<-","NeticaNode", function (node,value) {
  NodeUserObj(node,"lnAlphas") <- value
  node
})

setMethod("PnodeBetas","NeticaNode", function (node) {
  NodeUserObj(node,"betas")
})

setMethod("PnodeBetas<-","NeticaNode", function (node,value) {
  NodeUserObj(node,"betas") <- value
  node
})


setMethod("PnodeLinkScale","NeticaNode", function (node) {
  NodeUserObj(node,"linkScale")
})

setMethod("PnodeLinkScale<-","NeticaNode", function (node,value) {
  NodeUserObj(node,"linkScale") <- value
  node
})

setMethod("PnodePriorWeight","NeticaNode", function (node) {
  NodeUserObj(node,"priorWeight")
})

setMethod("PnodePriorWeight<-","NeticaNode", function (node,value) {
  NodeUserObj(node,"priorWeight") <- value
  node
})

setMethod("PnodePostWeight","NeticaNode", function (node) {
  NodeExperience(node)
})

setMethod("PnodeProbs","NeticaNode", function (node) {
  NodeProbs(node)
})

setMethod("PnodeProbs<-","NeticaNode", function (node,value) {
  NodeProbs(node) <- value
  node
})


##'Fetches a list of numeric variables corresponding to parent states
##'
##'
##'In constructing a conditional probability table using the discrete partial
##'credit framework (see \code{\link[CPTtools]{calcDPCTable}}), each state of
##'each parent variable is mapped onto a real value called the effective theta.
##'The \code{PnodeParentTvals} method for Netica nodes returns the result of
##'applying \code{\link[RNetica]{NodeLevels}} to each of the nodes in
##'\code{\link[RNetica]{NodeParents}(node)}.
##'
##'
##'While the best practices for assigning values to the states of the parent
##'nodes is probably to assign equal spaced values (using the function
##'\code{\link[CPTtools]{effectiveThetas}} for this purpose), this method needs
##'to retain some flexibility for other possibilities.  However, in general,
##'the best choice should depend on the meaning of the parent variable, and the
##'same values should be used everywhere the parent variable occurs.
##'
##'Netica already provides the \code{\link[RNetica]{NodeLevels}} function which
##'allows the states of a \code{\link[RNetica]{NeticaNode}} to be associated
##'with numeric values.  This method merely gathers them together.  The method
##'assumes that all of the parent variables have had their
##'\code{\link[RNetica]{NodeLevels}} set and will generate an error if that is
##'not true.
##'
##'@aliases PnodeParentTvals,NeticaNode-method
##'@param node A \code{\link{Pnode}} which is also a
##'\code{\link[RNetica]{NeticaNode}}.
##'@return
##'
##'\code{PnodeParentTvals(node)} should return a list corresponding to the
##'parents of \code{node}, and each element should be a numeric vector
##'corresponding to the states of the appropriate parent variable.  If there
##'are no parent variables, this will be a list of no elements.
##'@note
##'
##'The implementation is merely: \code{lapply(NodeParents(node), NodeLevels)}.
##'@author Russell Almond
##'@seealso \code{\link{Pnode.NeticaNode}}, \code{\link[Peanut]{Pnode}},
##'\code{\link[CPTtools]{effectiveThetas}},
##'\code{\link{BuildTable,NeticaNode-method}},
##'\code{\link{maxCPTParam,NeticaNode-method}}
##'@references
##'
##'Almond, R. G. (2015) An IRT-based Parameterization for Conditional
##'Probability Tables.  Paper presented at the 2015 Bayesian Application
##'Workshop at the Uncertainty in Artificial Intelligence Conference.
##'
##'Almond, R.G., Mislevy, R.J., Steinberg, L.S., Williamson, D.M. and Yan, D.
##'(2015) \emph{Bayesian Networks in Educational Assessment.} Springer.
##'Chapter 8.
##'@keywords attrib
##'@examples
##'
##'sess <- NeticaSession()
##'startSession(sess)
##'tNet <- CreateNetwork("TestNet", session=sess)
##'
##'theta1 <- NewDiscreteNode(tNet,"theta1",
##'                         c("VH","High","Mid","Low","VL"))
##'## This next function sets the effective thetas for theta1
##'NodeLevels(theta1) <- effectiveThetas(NodeNumStates(theta1))
##'NodeProbs(theta1) <- rep(1/NodeNumStates(theta1),NodeNumStates(theta1))
##'theta2 <- NewDiscreteNode(tNet,"theta2",
##'                         c("High","Mid","Low"))
##'## This next function sets the effective thetas for theta2
##'NodeLevels(theta2) <- effectiveThetas(NodeNumStates(theta2))
##'NodeProbs(theta2) <- rep(1/NodeNumStates(theta2),NodeNumStates(theta2))
##'
##'partial3 <- NewDiscreteNode(tNet,"partial3",
##'                            c("FullCredit","PartialCredit","NoCredit"))
##'NodeParents(partial3) <- list(theta1,theta2)
##'
##'## Usual way to set rules is in constructor
##'partial3 <- Pnode(partial3,rules="Compensatory", link="partialCredit")
##'
##'PnodeParentTvals(partial3)
##'do.call("expand.grid",PnodeParentTvals(partial3))
##'
##'DeleteNetwork(tNet)
##'stopSession(sess)
##'@exportMethod
setMethod("PnodeParentTvals","NeticaNode", function (node) {
  lapply(NodeParents(node),PnodeStateValues)
})

setMethod("Pnode","NeticaNode",
          function (node, lnAlphas, betas, rules="Compensatory",
                           link="partialCredit",Q=TRUE,linkScale=NULL,
                           priorWeight=NULL) {
  if (missing(lnAlphas)) {
    if (is.list(rules)) {
      lnAlphas <- lapply(rules, function(rule) defaultAlphas(node,rule))
    } else {
      lnAlphas <- defaultAlphas(node,rules)
    }
  }
  PnodeLnAlphas(node) <- lnAlphas
  if (missing(betas)) {
    if (is.list(rules)) {
      betas <- lapply(rules, function(rule) defaultBetas(node,rule))
    } else {
      betas <- defaultBetas(node,rules)
    }
  }
  PnodeBetas(node) <- betas
  PnodeRules(node) <- rules
  PnodeLink(node) <- link
  PnodeQ(node) <- Q
  PnodeLinkScale(node) <- linkScale
  PnodePriorWeight(node) <- priorWeight
  as.Pnode(node)
})


### Build CPTs from parameters
##'Builds the conditional probability table for a Pnode
##'
##'
##'The function \code{BuildTable} calls \code{\link[CPTtools]{calcDPCFrame}} to
##'calculate the conditional probability for a \code{\link{Pnode}} object, and
##'sets the current conditional probability table of \code{node} to the
##'resulting value.  It also sets the
##'\code{\link[RNetica]{NodeExperience}(node)} to the current value of
##'\code{\link[Peanut]{GetPriorWeight}(node)}.
##'
##'
##'The fields of the \code{\link{Pnode}} object correspond to the arguments of
##'the \code{\link[CPTtools]{calcDPCTable}} function.  The output conditional
##'probability table is then set in the node object in using the \code{[]}
##'(\link[RNetica]{Extract.NeticaNode}) operator.
##'
##'In addition to setting the CPT, the weight given to the nodes in the EM
##'algorithm are set to \code{\link[Peanut]{GetPriorWeight}(node)}, which will
##'extract the value of \code{\link[Peanut]{PnodePriorWeight}(node)} or if that
##'is null, the value of
##'\code{\link[Peanut]{PnetPriorWeight}(\link[RNetica]{NodeParents}(node))} and
##'set \code{\link[RNetica]{NodeExperience}(node)} to the resulting value.
##'
##'@aliases BuildTable,NeticaNode-method
##'@param node A \code{\link[Peanut]{Pnode}} and
##'\code{\link[RNetica]{NeticaNode}} object whose table is to be built.
##'@return
##'
##'The \code{node} argument is returned invisibly.  As a side effect the
##'conditional probability table and experience of \code{node} is modified.
##'@author Russell Almond
##'@seealso \code{\link{Pnode.NeticaNode}}, \code{\link[Peanut]{Pnode}},
##'\code{\link[Peanut]{PnodeQ}}, \code{\link[Peanut]{PnodePriorWeight}},
##'\code{\link[Peanut]{PnodeRules}}, \code{\link[Peanut]{PnodeLink}},
##'\code{\link[Peanut]{PnodeLnAlphas}}, \code{\link[Peanut]{PnodeAlphas}},
##'\code{\link[Peanut]{PnodeBetas}},
##'\code{\link[Peanut]{PnodeLinkScale}},\code{\link[Peanut]{GetPriorWeight}},
##'\code{\link[CPTtools]{calcDPCTable}},
##'\code{\link[RNetica]{NodeExperience}(node)},
##'\code{\link[RNetica]{Extract.NeticaNode}} (\code{[}) %]
##'@references
##'
##'Almond, R. G. (2015) An IRT-based Parameterization for Conditional
##'Probability Tables.  Paper presented at the 2015 Bayesian Application
##'Workshop at the Uncertainty in Artificial Intelligence Conference.
##'@keywords distribution
##'@examples
##'
##'
##'sess <- NeticaSession()
##'startSession(sess)
##'
##'## Network with two proficiency variables and observables for each
##'## different type of rule
##'
##'binAll <- CreateNetwork("binAll", session=sess)
##'PnetPriorWeight(binAll) <- 11           #Give it something to see.
##'
##'## Set up Proficiency Model.
##'thetas <- NewDiscreteNode(binAll,paste("theta",0:1,sep=""),
##'                          c("Low","Med","High")) # Create the variable with 3 levels
##'names(thetas) <- paste("theta",0:1,sep="")
##'NodeParents(thetas[[2]]) <- thetas[1]
##'
##'for (nd in thetas) {
##'  NodeLevels(nd) <- effectiveThetas(NodeNumStates(nd))
##'  PnodeRules(nd) <- "Compensatory"
##'  PnodeLink(nd) <- "normalLink"
##'  PnodeBetas(nd) <- 0 # A numeric vector of intercept parameters
##'  PnodeQ(nd) <- TRUE # All parents are relevant.
##'  NodeSets(nd) <- c("pnodes","Proficiency") # A character vector
##'                   # containing the names of the node sets
##'}
##'
##'## Standard normal prior.
##'PnodeAlphas(thetas[[1]]) <- numeric() # A numeric vector of (log) slope parameters
##'PnodeLinkScale(thetas[[1]]) <- 1 # A positive numeric value, or NULL
##'                                 # if the scale parameter is not used
##'                                 # for the link function.
##'## Regression with a correlation of .6
##'PnodeAlphas(thetas[[2]]) <- .6
##'PnodeLinkScale(thetas[[2]]) <- .8
##'
##'BuildTable(thetas[[1]])
##'BuildAllTables(binAll)
##'
##'DeleteNetwork(binAll)
##'stopSession(sess)
##'
##' @exportMethod 
setMethod("BuildTable","NeticaNode", function (node) {
  if (length(PnodeBetas(node)) == 0L) {
    flog.warn("Beta vector for node %s is empty.",PnodeName(node))
  }
  if (length(PnodeAlphas(node)) == 0L) {
    flog.warn("Alpha vector for node %s is empty.",PnodeName(node))
  }
  frame <- calcDPCFrame(ParentStates(node),NodeStates(node),
                          PnodeLnAlphas(node), PnodeBetas(node),
                          PnodeRules(node),PnodeLink(node),
                          PnodeLinkScale(node),PnodeQ(node),
                        PnodeParentTvals(node))
  if (any(is.na(frame))) {
    flog.warn("Could not calculate CPT for node %s.",PnodeName(node))
  } else {
    node[] <- frame
  }
  NodeExperience(node) <- GetPriorWeight(node)
  invisible(node)
})

##'Calculates the log likelihood for a set of data under a Pnet.NeticaBN model
##'
##'
##'The method \code{calcPnetLLike.NeticaBN} calculates the log likelihood for a
##'set of data contained in \code{cases} using the current conditional
##'probability tables in a \code{\link{Pnet.NeticaBN}}.  Here \code{cases}
##'should be the filename of a Netica case file (see
##'\code{\link[RNetica]{write.CaseFile}}).
##'
##'
##'This function provides the convergence test for the \code{\link{GEMfit}}
##'algorithm.  The \code{\link{Pnet.NeticaBN}} represents a model (with
##'parameters set to the value used in the current iteration of the EM
##'algorithm) and \code{cases} a set of data.  This function gives the log
##'likelihood of the data.
##'
##'This method expects the \code{cases} argument to be a pathname pointing to a
##'Netica cases file containing the training or test data (see
##'\code{\link[RNetica]{write.CaseFile}}).  Also, it expects that there is a
##'nodeset (see \code{\link[RNetica]{NetworkNodesInSet}}) attached to the
##'network called \dQuote{onodes} which references the observable variables in
##'the case file.
##'
##'As Netica does not have an API function to directly calculate the
##'log-likelihood of a set of cases, this method loops through the cases in the
##'case set and calls \code{\link[RNetica]{FindingsProbability}(net)} for each
##'one.  Note that if there are frequencies in the case file, each case is
##'weighted by its frequency.
##'
##'@aliases calcPnetLLike,NeticaBN-method
##'@param net A \code{\link{Pnet.NeticaBN}} object representing a parameterized
##'network.
##'@param cases A character scalar giving the file name of a Netica case file
##'(see \code{\link[RNetica]{write.CaseFile}}).
##'@return
##'
##'A numeric scalar giving the log likelihood of the data in the case file.
##'@author Russell Almond
##'@seealso
##'
##'\code{\link[Peanut]{Pnet}}, \code{\link{Pnet.NeticaBN}},
##'\code{\link[Peanut]{GEMfit}}, \code{\link[Peanut]{calcExpTables}},
##'\code{\link[Peanut]{BuildAllTables}},
##'\code{\link[Peanut]{maxAllTableParams}}
##'\code{\link[RNetica]{NetworkNodesInSet}},
##'\code{\link[RNetica]{FindingsProbability}},
##'\code{\link[RNetica]{write.CaseFile}}
##'@references
##'
##'Almond, R. G. (2015) An IRT-based Parameterization for Conditional
##'Probability Tables.  Paper presented at the 2015 Bayesian Application
##'Workshop at the Uncertainty in Artificial Intelligence Conference.
##'@keywords graphs
##'@examples
##'
##'sess <- NeticaSession()
##'startSession(sess)
##'
##'irt10.base <- ReadNetworks(file.path(library(help="PNetica")$path,
##'                           "testnets","IRT10.2PL.base.dne"), session=sess)
##'irt10.base <- as.Pnet(irt10.base)  ## Flag as Pnet, fields already set.
##'irt10.theta <- NetworkFindNode(irt10.base,"theta")
##'irt10.items <- PnetPnodes(irt10.base)
##'## Flag items as Pnodes
##'for (i in 1:length(irt10.items)) {
##'  irt10.items[[i]] <- as.Pnode(irt10.items[[i]])
##'}
##'CompileNetwork(irt10.base) ## Netica requirement
##'
##'casepath <- file.path(library(help="PNetica")$path,
##'                           "testdat","IRT10.2PL.200.items.cas")
##'## Record which nodes in the casefile we should pay attention to
##'NetworkNodesInSet(irt10.base,"onodes") <-
##'   NetworkNodesInSet(irt10.base,"observables")
##'
##'llike <- calcPnetLLike(irt10.base,casepath)
##'
##'DeleteNetwork(irt10.base)
##'stopSession(sess)
##'
##'
##' @exportMethod
setMethod("calcPnetLLike","NeticaBN", function (net,cases){
  llike <- 0
  nextRec <- "FIRST"
  onodes <- NetworkNodesInSet(net,"onodes")
  pos <- 0
  stream <- CaseFileStream(cases,net$Session)
  WithOpenCaseStream(stream,
    while(!is.na(pos)) {
      ReadFindings(onodes,stream,nextRec)
      nextRec <- "NEXT"
      pos <- getCaseStreamPos(stream)
      w <- getCaseStreamLastFreq(stream)
      if (w<0) w<-1
      llike <- llike + w*log(FindingsProbability(net))
      lapply(onodes,RetractNodeFinding)
    })
  llike
})

##'Calculate expected tables for a Pnet.NeticaBN
##'
##'
##'The performs the E-step of the GEM algorithm by running the Netica EM
##'algorithm (see \code{\link[RNetica]{LearnCPTs}}) using the data in
##'\code{cases}.  After this is run, the conditional probability table for each
##'\code{\link{Pnode.NeticaNode}} should be the mean of the Dirichlet
##'distribution and the scale parameter should be the value of
##'\code{\link[RNetica]{NodeExperience}(node)}.
##'
##'
##'The key to this method is realizing that the EM algorithm built into the
##'Netica (see \code{\link[RNetica]{LearnCPTs}}) can perform the E-step of the
##'outer \code{\link[Peanut]{GEMfit}} generalized EM algorithm.  It does this
##'in every iteration of the algorithm, so one can stop after the first
##'iteration of the internal EM algorithm.
##'
##'This method expects the \code{cases} argument to be a pathname pointing to a
##'Netica cases file containing the training or test data (see
##'\code{\link[RNetica]{write.CaseFile}}).  Also, it expects that there is a
##'nodeset (see \code{\link[RNetica]{NetworkNodesInSet}}) attached to the
##'network called \dQuote{onodes} which references the observable variables in
##'the case file.
##'
##'Before calling this method, the function \code{\link{BuildTable}} needs to
##'be called on each \code{Pnode} to both ensure that the conditional
##'probability table is at a value reflecting the current parameters and to
##'reset the value of \code{\link[RNetica]{NodeExperience}(node)} to the
##'starting value of \code{\link{GetPriorWeight}(node)}.
##'
##'Note that Netica does allow \code{\link[RNetica]{NodeExperience}(node)} to
##'have a different value for each row the the conditional probability table.
##'However, in this case, each node must have its own prior weight (or exactly
##'the same number of parents).  The prior weight counts as a number of cases,
##'and should be scaled appropriately for the number of cases in \code{cases}.
##'
##'The parameters \code{Estepit} and \code{tol} are passed
##'\code{\link[RNetica]{LearnCPTs}}.  Note that the outer EM algorithm assumes
##'that the expected table counts given the current values of the parameters,
##'so the default value of one is sufficient.  (It is possible that a higher
##'value will speed up convergence, the parameter is left open for
##'experimentation.)  The tolerance is largely irrelevant as the outer EM
##'algorithm does the tolerance test.
##'
##'@aliases calcExpTables,NeticaBN-method
##'@param net A \code{\link{Pnet.NeticaBN}} object representing a parameterized
##'network.
##'@param cases A character scalar giving the file name of a Netica case file
##'(see \code{\link[RNetica]{write.CaseFile}}).
##'@param Estepit An integer scalar describing the number of steps the Netica
##'should take in the internal EM algorithm.
##'@param tol A numeric scalar giving the stopping tolerance for the internal
##'Netica EM algorithm.
##'@return
##'
##'The \code{net} argument is returned invisibly.
##'
##'As a side effect, the internal conditional probability tables in the network
##'are updated as are the internal weights given to each row of the conditional
##'probability tables.
##'@author Russell Almond
##'@seealso
##'
##'\code{\link[Peanut]{Pnet}}, \code{\link{Pnet.NeticaBN}},
##'\code{\link[Peanut]{GEMfit}}, \code{\link[Peanut]{calcPnetLLike}},
##'\code{\link[Peanut]{maxAllTableParams}},
##'\code{\link[Peanut]{calcExpTables}},
##'\code{\link[RNetica]{NetworkNodesInSet}}
##'\code{\link[RNetica]{write.CaseFile}}, \code{\link[RNetica]{LearnCPTs}}
##'@references
##'
##'Almond, R. G. (2015) An IRT-based Parameterization for Conditional
##'Probability Tables.  Paper presented at the 2015 Bayesian Application
##'Workshop at the Uncertainty in Artificial Intelligence Conference.
##'@keywords manip
##'@examples
##'
##'sess <- NeticaSession()
##'startSession(sess)
##'
##'irt10.base <- ReadNetworks(file.path(library(help="PNetica")$path,
##'                           "testnets","IRT10.2PL.base.dne"), session=sess)
##'irt10.base <- as.Pnet(irt10.base)  ## Flag as Pnet, fields already set.
##'irt10.theta <- NetworkFindNode(irt10.base,"theta")
##'irt10.items <- PnetPnodes(irt10.base)
##'## Flag items as Pnodes
##'for (i in 1:length(irt10.items)) {
##'  irt10.items[[i]] <- as.Pnode(irt10.items[[i]])
##'}
##'CompileNetwork(irt10.base) ## Netica requirement
##'
##'casepath <- file.path(library(help="PNetica")$path,
##'                           "testdat","IRT10.2PL.200.items.cas")
##'## Record which nodes in the casefile we should pay attention to
##'NetworkNodesInSet(irt10.base,"onodes") <-
##'   NetworkNodesInSet(irt10.base,"observables")
##'
##'item1 <- irt10.items[[1]]
##'
##'priorcounts <- sweep(NodeProbs(item1),1,NodeExperience(item1),"*")
##'
##'calcExpTables(irt10.base,casepath)
##'
##'postcounts <- sweep(NodeProbs(item1),1,NodeExperience(item1),"*")
##'
##'## Posterior row sums should always be larger.
##'stopifnot(
##'  all(apply(postcounts,1,sum) >= apply(priorcounts,1,sum))
##')
##'
##'DeleteNetwork(irt10.base)
##'stopSession(sess)
##'
##' @exportMethod 
setMethod("calcExpTables","NeticaBN", function (net, cases, Estepit=1,
                                    tol=sqrt(.Machine$double.eps)) {
  pnodes <- NetworkNodesInSet(net,"pnodes")
  casestream <- CaseFileStream(cases,session=net$Session)
  LearnCPTs(casestream,pnodes,"EM",Estepit,tol)
  invisible(net)
})


## This function is designed to suppress lack of convergence warnings,
## as we are frequently not wanting to run the M-step to convergence.
muffler <- function (w) {
  if (conditionMessage(w) == "" ||
      grepl("converge",conditionMessage(w)))
    invokeRestart("muffleWarning")
}

##'Find optimal parameters of a Pnode.NeticaNode to match expected tables
##'
##'
##'These function assumes that an expected count contingency table can be built
##'from the network; i.e., that \code{\link[RNetica]{LearnCPTs}} has been
##'recently called.  They then try to find the set of parameters maximizes the
##'probability of the expected contingency table with repeated calls to
##'\code{\link[CPTtools]{mapDPC}}.  This describes the method for
##'\code{\link[Peanut]{maxCPTParam}} when the \code{\link[Peanut]{Pnode}} is a
##'\code{\link[RNetica]{NeticaNode}}.
##'
##'
##'This method is called on on a \code{\link{Pnode.NeticaNode}} object during
##'the M-step of the EM algorithm (see \code{\link[Peanut]{GEMfit}} and
##'\code{\link[Peanut]{maxAllTableParams}} for details).  Its purpose is to
##'extract the expected contingency table from Netica and pass it along to
##'\code{\link[CPTtools]{mapDPC}}.
##'
##'When doing EM learning with Netica, the resulting conditional probability
##'table (CPT) is the mean of the Dirichlet posterior.  Going from the mean to
##'the parameter requires multiplying the CPT by row counts for the number of
##'virtual observations.  In Netica, these are call
##'\code{\link[RNetica]{NodeExperience}}.  Thus, the expected counts are
##'calculated with this expression: \code{sweep(node[[]], 1,
##'NodeExperience(node), "*")}.
##'
##'What remains is to take the table of expected counts and feed it into
##'\code{\link[CPTtools]{mapDPC}} and then take the output of that routine and
##'update the parameters.
##'
##'The parameters \code{Mstepit} and \code{tol} are passed to
##'\code{\link[CPTtools]{mapDPC}} to control the gradient decent algorithm used
##'for maximization.  Note that for a generalized EM algorithm, the M-step does
##'not need to be run to convergence, a couple of iterations are sufficient.
##'The value of \code{Mstepit} may influence the speed of convergence, so the
##'optimal value may vary by application.  The tolerance is largely irrelevant
##'(if \code{Mstepit} is small) as the outer EM algorithm does the tolerance
##'test.
##'
##'@aliases maxCPTParam,NeticaNode-method
##'@param node A \code{\link{Pnode}} object giving the parameterized node.
##'@param Mstepit A numeric scalar giving the number of maximization steps to
##'take.  Note that the maximization does not need to be run to convergence.
##'@param tol A numeric scalar giving the stopping tolerance for the maximizer.
##'@return
##'
##'The expression \code{maxCPTParam(node)} returns \code{node} invisibly.  As a
##'side effect the \code{\link{PnodeLnAlphas}} and \code{\link{PnodeBetas}}
##'fields of \code{node} (or all nodes in \code{\link{PnetPnodes}(net)}) are
##'updated to better fit the expected tables.
##'@author Russell Almond
##'@seealso
##'
##'\code{\link[Peanut]{Pnode}}, \code{\link{Pnode.NeticaNode}},
##'\code{\link[Peanut]{GEMfit}}, \code{\link[Peanut]{maxAllTableParams}}
##'\code{\link[CPTtools]{mapDPC}}
##'@references
##'
##'Almond, R. G. (2015) An IRT-based Parameterization for Conditional
##'Probability Tables.  Paper presented at the 2015 Bayesian Application
##'Workshop at the Uncertainty in Artificial Intelligence Conference.
##'@keywords manip
##'@examples
##'
##'
##'## This method is mostly a wrapper for CPTtools::mapDPC
##'getMethod(maxCPTParam,"NeticaNode")
##'
##'
##'sess <- NeticaSession()
##'startSession(sess)
##'
##'irt10.base <- ReadNetworks(paste(library(help="PNetica")$path,
##'                           "testnets","IRT10.2PL.base.dne",
##'                           sep=.Platform$file.sep),
##'                           session=sess)
##'irt10.base <- as.Pnet(irt10.base)  ## Flag as Pnet, fields already set.
##'irt10.theta <- NetworkFindNode(irt10.base,"theta")
##'irt10.items <- PnetPnodes(irt10.base)
##'## Flag items as Pnodes
##'for (i in 1:length(irt10.items)) {
##'  irt10.items[[i]] <- as.Pnode(irt10.items[[i]])
##'  ## Add node to list of observed nodes
##'  PnodeLabels(irt10.items[[1]]) <-
##'     union(PnodeLabels(irt10.items[[1]]),"onodes")
##'}
##'
##'casepath <- paste(library(help="PNetica")$path,
##'                           "testdat","IRT10.2PL.200.items.cas",
##'                           sep=.Platform$file.sep)
##'
##'
##'BuildAllTables(irt10.base)
##'PnetCompile(irt10.base) ## Netica requirement
##'
##'item1 <- irt10.items[[1]]
##'priB <- PnodeBetas(item1)
##'priA <- PnodeAlphas(item1)
##'priCPT <- PnodeProbs(item1)
##'
##'gemout <- GEMfit(irt10.base,casepath,trace=TRUE)
##'
##'calcExpTables(irt10.base,casepath)
##'
##'maxAllTableParams(irt10.base)
##'
##'postB <- PnodeBetas(item1)
##'postA <- PnodeAlphas(item1)
##'BuildTable(item1)
##'postCPT <- PnodeProbs(item1)
##'
##'## Posterior should be different
##'stopifnot(
##'  postB != priB, postA != priA
##')
##'
##'
##'DeleteNetwork(irt10.base)
##'stopSession(sess)
##'
##'
##'@exportMethod
setMethod("maxCPTParam","NeticaNode", function (node, Mstepit=5,
                                    tol=sqrt(.Machine$double.eps)) {
  ## Get the posterior pseudo-counts by multiplying each row of the
  ## node's CPT by its experience.
  ne <- NodeExperience(node)
  if (any(is.na(ne)) || is.null(ne)) ne <- GetPriorWeight(node)
  np <- length(dim(NodeProbs(node)))-1L
  if (np==0L || length(ne) == 1L) {
    counts <- NodeProbs(node)*ne
  } else {
    counts <- sweep(NodeProbs(node),1L:np,ne,"*")
  }
  withCallingHandlers(
      est <- CPTtools::mapDPC(counts,ParentStates(node),NodeStates(node),
                    PnodeLnAlphas(node), PnodeBetas(node),
                    PnodeRules(node),PnodeLink(node),
                    PnodeLinkScale(node),PnodeQ(node),
                    PnodeParentTvals(node),
                    control=list(reltol=tol,maxit=Mstepit)
                    ),
      warning=muffler)
  PnodeLnAlphas(node) <- est$lnAlphas
  PnodeBetas(node) <- est$betas
  PnodeLinkScale(node) <- est$linkScale
  invisible(node)
})

### Implementation of the factory protocol

## No-op for now.  Explicitly call delete.
Free.NeticaNode <-function (obj) {
  invisible(NULL)
}

Delete.NeticaNode <- function (obj) {
  DeleteNodes(obj)
}





##'Makes a Pnode which is also a Netica Node
##'
##'
##'This does the actual work of making a node from a warehose manifest.  It is
##'typically called from \code{\link[Peanut]{WarehouseMake}}.
##'
##'
##'This is a key piece of the \code{\link[Peanut]{Warehouse}} infrastructure.
##'If a node of the designated name does not exist, it will be created.  If it
##'does exist, the metadata fields of the node will be adjusted to match the
##'fields in the \code{data} object.
##'
##'Some of the fields of the \code{data} object apply to the whole node.  In
##'these fields, the value in the first row is used and the rest are ignored.
##'
##'\describe{ \item{NStates}{A integer giving the number of states for a
##'discrete variable or the discritzation of a continuous one.  The number of
##'rows of the data frame should match this.} \item{Continuous}{A logical value
##'telling whether or not the node should be regarded as continuous.}
##'\item{NodeTitle}{This is a longer unconstrained name for the node.}
##'\item{NodeDescription}{This is a longer string describing the node.}
##'\item{NodeLabels}{This is a comma separated list of tags identifying sets to
##'which the node belongs.  See \code{\link[Peanut]{PnodeLabels}}.} }
##'
##'These fields are repeated for each of the states in the node, as they are
##'different for each state.
##'
##'\describe{ \item{StateName}{The name of the state, this should follow the
##'Netica \link[RNetica]{IDname} conventions.} \item{StateTitle}{This is a
##'longer unconstrained name for the state.} \item{StateDescription}{This is a
##'longer string describing the state.} }
##'
##'Additionally, the following field is used only for discrete nodes:
##'\describe{ \item{StateValue}{This is a numeric value assigned to the state.
##'This value is used when calculating the node expected value.} } The
##'StateValue plays two important roles.  First, when used with the
##'\code{\link[Peanut]{PnodeEAP}} and \code{\link[Peanut]{PnodeSD}} functions,
##'it is the value assigned to the node.  Second, when constructing CPTs using
##'the DiBello framework, it is used at the effective thetas.  See
##'\code{\link[Peanut]{PnodeParentTvals}} and
##'\code{\link[Peanut]{PnodeStateValues}}
##'
##'Continuous nodes in Netica are handled by breaking the interval up into
##'pieces.  This is the function \code{\link[Peanut]{PnodeStateBounds}}.  Note
##'that the bounds should be either monotonically increasing or decreasing and
##'that the lower bound for one category should match lower bound for the next
##'to within a tolerance of .002.  The values \code{Inf} and \code{-Inf} can be
##'used where appropriate.
##'
##'\describe{ \item{LowerBound}{This is a numeric value giving the lower bound
##'for the range for the discritization of the node.} \item{UpperBound}{This is
##'a numeric value giving the upper bound for the range for the} }
##'
##'@param net A \code{\link[RNetica]{NeticaBN}} object in which the node will
##'be created.
##'@param name The name of the node.  Ideally, this should follow the Netica
##'\link[RNetica]{IDname} rules.
##'@param data A \code{data.frame} with one for each state of contains data and
##'meta-data about the node and states (See details).
##'@return
##'
##'An object of class \code{\link[RNetica]{NeticaNode}} which is also in the
##'\code{\link[Peanut]{Pnode}} abtract class.
##'@section Names and Truenames:
##'
##'The truename system is designed to implement the name restrictions inherent
##'in Netica (see \link[RNetica]{IDname}) without imposing the same limits on
##'the Peanut framework.  This is done by adding a \code{Truename} field to the
##'net object and then mangling the actual name to follow the Netica rules
##'using the \code{\link[RNetica]{as.IDname}} function.
##'
##'The object should be available from the warehouse via its truename, but it
##'is best to stick to the Netica naming conventions for networks and nodes.
##'
##'Note that the truename convention is used for node names, but not for state
##'names, which are restricted to Netica conventions.
##'@author Russell Almond
##'@seealso RNetica Package: \code{\link[RNetica]{NeticaNode}},
##'\code{\link[RNetica]{NewContinuousNode}},
##'\code{\link[RNetica]{NewDiscreteNode}}, \code{\link[RNetica]{IDname}}
##'
##'Peanut Package: \code{\link[Peanut]{Warehouse}},
##'\code{\link[Peanut]{WarehouseMake}}
##'
##'PNetica Pacakge \code{\link[Peanut]{PnodeWarehouse}}
##'@keywords manip graphs
##'@examples
##'
##'sess <- NeticaSession()
##'startSession(sess)
##'
##'### This tests the manifest and factory protocols.
##'
##'netman1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
##'                          "Mini-PP-Nets.csv", sep=.Platform$file.sep),
##'                    row.names=1, stringsAsFactors=FALSE)
##'## Build the first network (proficiency model)
##'miniPP <- MakePnet.NeticaBN(sess,"miniPP",netman1[1,,drop=FALSE])
##'
##'nodeman1 <- read.csv(paste(library(help="Peanut")$path, "auxdata",
##'                           "Mini-PP-Nodes.csv", sep=.Platform$file.sep),
##'                     row.names=1,stringsAsFactors=FALSE)
##'
##'## Discrete Example
##'phys.dat <- nodeman1[nodeman1$NodeName=="Physics",]
##'
##'Physics <- MakePnode.NeticaNode(miniPP,"Physics",phys.dat)
##'
##'## Continuous Example
##'dur.dat <- nodeman1[nodeman1$NodeName=="Duration",]
##'
##'Duration <- MakePnode.NeticaNode(miniPP,"Duration",dur.dat)
##'
##'
##'DeleteNetwork(miniPP)
##'stopSession(sess)
##'
##'
##'@export MakePnode.NeticaNode
MakePnode.NeticaNode <- function (net, name, data) {
  if (nrow(data) != as.integer(data$Nstates[1]))
    stop("Must be one row in data for each state.")
  node <-PnetFindNode(net,name)
  cont <- isTRUE(as.logical(data$Continuous[1]))
  if (is.null(node)) {
    if (cont)
      node <- NewContinuousNode(net,as.IDname(name))
    else
      node <- NewDiscreteNode(net,as.IDname(name),
                              trimws(as.character(data$StateName)))
    NodeUserField(node,"Truename") <- name
  }
  node <- as.Pnode(node)
  if (!is.null(data$NodeTitle[1]) && !is.na(data$NodeTitle[1]))
    PnodeTitle(node) <- as.character(data$NodeTitle[1])
  if (!is.null(data$NodeDescription[1]) &&
      !is.na(data$NodeDescription[1]))
    PnodeDescription(node) <- as.character(data$NodeDescription[1])
  if (!is.null(data$NodeLabels[1]) && !is.na(data$NodeLabels[1])) {
    labels <- trimws(strsplit(data$NodeLabels[1],",")[[1]])
    PnodeLabels(node) <- as.character(labels)
  }
  if (cont) {
    ## Need to set values to create states.
    valmat <- cbind(as.numeric(data$LowerBound),
                    as.numeric(data$UpperBound))
    if (any(is.na(valmat))) {
      warning("NAs in states bounds for node",name)
    } else {
      PnodeStateBounds(node) <-valmat
    }
  }
  PnodeStates(node) <- trimws(as.character(data$StateName))
  if (!is.null(data$StateTitle)) {
    titles <- as.character(data$StateTitle)
    if (all(!is.na(titles)))
      PnodeStateTitles(node) <- titles
  }
  if (!is.null(data$StateDescription)) {
    desc <- as.character(data$StateDescription)
    if (all(!is.na(desc)))
      PnodeStateDescriptions(node) <- desc
  }
  if (!cont && !is.null(data$StateValue) && !any(is.na(data$StateValue)))
    PnodeStateValues(node) <- as.numeric(data$StateValue)

  node

}
