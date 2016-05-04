### Parameterized networks.


## Parameterized networks have the following properties:

## A node set called Pnodes which contains a list of all Pnodes to
## maximize.
## A field called "priorWeight" which gives the default prior weight
## to use.

as.Pnet.NeticaBN <- function (x) {
  if (!("Pnet" %in% class(x)))
    class(x) <- c(class(x),"Pnet")
  x
}

PnetPriorWeight.NeticaBN <- function (net) {
  NetworkUserObj(net,"priorWeight")
}

"PnetPriorWeight<-.NeticaBN" <- function (net,value) {
  NetworkUserObj(net,"priorWeight") <- value
  invisible(net)
}

PnetPnodes.NeticaBN <- function (net) {
  NetworkNodesInSet(net,"pnodes")
}
"PnetPnodes<-.NeticaBN" <- function (net, value) {
  NetworkNodesInSet(net,"pnodes") <- value
  invisible(net)
}


## A parameterized node has the following fields:

## rules -- the name of the structure function
## link -- the name of the link function
## lnAlphas -- a list of discrimination parameters
## betas -- a list of difficulty parameters
## linkScale -- a list of scale parameters
## priorWeight -- a numeric value or a vector of numeric values for
## each row of the CPT.   Inherits from the net if not available.

as.Pnode.NeticaNode <- function (x) {
  if (!("Pnode" %in% class(x)))
    class(x) <- c(class(x),"Pnode")
  x
}

PnodeNet.NeticaNode <- function (node) {
  NodeNet(node)
}

PnodeRules.NeticaNode <- function (node) {
  NodeUserObj(node,"rules")
}

"PnodeRules<-.NeticaNode" <- function (node,value) {
  NodeUserObj(node,"rules") <- value
  node
}

PnodeLink.NeticaNode <- function (node) {
  NodeUserObj(node,"link")
}

"PnodeLink<-.NeticaNode" <- function (node,value) {
  NodeUserObj(node,"link") <- value
  node
}

PnodeQ.NeticaNode <- function (node) {
  NodeUserObj(node,"Q")
}

"PnodeQ<-.NeticaNode" <- function (node,value) {
  NodeUserObj(node,"Q") <- value
  node
}

PnodeLnAlphas.NeticaNode <- function (node) {
  NodeUserObj(node,"lnAlphas")
}

"PnodeLnAlphas<-.NeticaNode" <- function (node,value) {
  NodeUserObj(node,"lnAlphas") <- value
  node
}

PnodeBetas.NeticaNode <- function (node) {
  NodeUserObj(node,"betas")
}

"PnodeBetas<-.NeticaNode" <- function (node,value) {
  NodeUserObj(node,"betas") <- value
  node
}


PnodeLinkScale.NeticaNode <- function (node) {
  NodeUserObj(node,"linkScale")
}

"PnodeLinkScale<-.NeticaNode" <- function (node,value) {
  NodeUserObj(node,"linkScale") <- value
  node
}

PnodePriorWeight.NeticaNode <- function (node) {
  NodeUserObj(node,"priorWeight")
}

"PnodePriorWeight<-.NeticaNode" <- function (node,value) {
  NodeUserObj(node,"priorWeight") <- value
  node
}

PnodeParentTvals.NeticaNode <- function (node) {
  lapply(NodeParents(node),NodeLevels)
}

Pnode.NeticaNode <- function (node, lnAlphas, betas, rules="Compensatory",
                           link="partialCredit",Q=TRUE,linkScale=NULL,
                           priorWeight=NULL) {
  if (!("Pnode" %in% class(node)))
    class(node) <- c(class(node),"Pnode")
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
  node
}


### Build CPTs from parameters

BuildTable.NeticaNode <- function (node) {
  node[] <- calcDPCFrame(ParentStates(node),NodeStates(node),
                          PnodeLnAlphas(node), PnodeBetas(node),
                          PnodeRules(node),PnodeLink(node),
                          PnodeLinkScale(node),PnodeQ(node),
                          PnodeParentTvals(node))
  NodeExperience(node) <- GetPriorWeight(node)
  invisible(node)
}


calcPnetLLike.NeticaBN <- function (net,cases){
  llike <- 0
  nextRec <- "FIRST"
  onodes <- NetworkNodesInSet(net,"onodes")
  pos <- 0
  stream <- CaseFileStream(cases)
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
}

calcExpTables.NeticaBN <- function (net, cases, Estepit=1,
                                    tol=sqrt(.Machine$double.eps)) {
  pnodes <- NetworkNodesInSet(net,"pnodes")
  LearnCPTs(cases,pnodes,"EM",Estepit,tol)
  invisible(net)
}


## This function is designed to suppress lack of convergence warnings,
## as we are frequently not wanting to run the M-step to convergence.
muffler <- function (w) {
  if (conditionMessage(w) == "" ||
      grepl("converge",conditionMessage(w)))
    invokeRestart("muffleWarning")
}


maxCPTParam.NeticaNode <- function (node, Mstepit=5,
                                    tol=sqrt(.Machine$double.eps)) {
  ## Get the posterior pseudo-counts by multiplying each row of the
  ## node's CPT by its experience.
  counts <- sweep(NodeProbs(node),1,NodeExperience(node),"*")
  withCallingHandlers(
      est <- mapDPC(counts,ParentStates(node),NodeStates(node),
                    PnodeLnAlphas(node), PnodeBetas(node),
                    PnodeRules(node),PnodeLink(node),
                    PnodeLinkScale(node),PnodeQ(node),
                    control=list(reltol=tol,maxit=Mstepit)
                    ),
      warning=muffler)
  PnodeLnAlphas(node) <- est$lnAlphas
  PnodeBetas(node) <- est$betas
  PnodeLinkScale(node) <- est$linkScale
  invisible(node)
}
