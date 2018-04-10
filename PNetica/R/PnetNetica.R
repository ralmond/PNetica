### Parameterized networks.


## Parameterized networks have the following properties:

## A node set called Pnodes which contains a list of all Pnodes to
## maximize.
## A field called "priorWeight" which gives the default prior weight
## to use.


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

MakePnet.NeticaBN <-function (sess,name,data) {
  pname <- as.character(data$Pathname)
  if (!is.null(pname) && file.exists(pname)) {
    net <-as.Pnet(ReadNetworks(pname,sess))
    PnetName(net) <- name
  } else {
    net <- as.Pnet(CreateNetwork(name,sess))
  }
  if (!is.null(data$Hub))
    PnetHub(net) <- as.character(data$Hub)
  if (!is.null(data$Title))
    PnetTitle(net) <- as.character(data$Title)
  if (!is.null(data$Pathname))
    PnetPathname(net) <- as.character(data$Pathname)
  if (!is.null(data$Description))
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
  CopyNodes(nodes,newnet=net)
})

## Deleting the node makes it a stub.
setMethod("PnetRemoveStubNodes","NeticaBN", function (net,nodes) {
  DeleteNodes(nodes)
})

setMethod("PnetAdjoin","NeticaBN", function (hub, spoke) {
  AdjoinNetwork(hub,spoke,paste("Spoke",NetworkName(spoke),sep="_"))
})

setMethod("PnetDetach","NeticaBN", function (motif, spoke) {
  # Bug in RN_AbsorbNodes
  # AbsorbNodes(NetworkNodesInSet(motif,paste("Spoke",NetworkName(spoke),sep="_")))
})




## A parameterized node has the following fields:

## rules -- the name of the structure function
## link -- the name of the link function
## lnAlphas -- a list of discrimination parameters
## betas -- a list of difficulty parameters
## linkScale -- a list of scale parameters
## priorWeight -- a numeric value or a vector of numeric values for
## each row of the CPT.   Inherits from the net if not available.


setMethod("as.Pnode","NeticaNode",function(x) x)
setMethod("is.Pnode","NeticaNode",function(x) TRUE)


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

setMethod("PnodeParentTvals","NeticaNode", function (node) {
  lapply(NodeParents(node),NodeLevels)
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
  node
})


### Build CPTs from parameters

setMethod("BuildTable","NeticaNode", function (node) {
  node[] <- calcDPCFrame(ParentStates(node),NodeStates(node),
                          PnodeLnAlphas(node), PnodeBetas(node),
                          PnodeRules(node),PnodeLink(node),
                          PnodeLinkScale(node),PnodeQ(node),
                          PnodeParentTvals(node))
  NodeExperience(node) <- GetPriorWeight(node)
  invisible(node)
})


setMethod("calcPnetLLike","NeticaBN", function (net,cases){
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
})

setMethod("calcExpTables","NeticaBN", function (net, cases, Estepit=1,
                                    tol=sqrt(.Machine$double.eps)) {
  pnodes <- NetworkNodesInSet(net,"pnodes")
  LearnCPTs(cases,pnodes,"EM",Estepit,tol)
  invisible(net)
})


## This function is designed to suppress lack of convergence warnings,
## as we are frequently not wanting to run the M-step to convergence.
muffler <- function (w) {
  if (conditionMessage(w) == "" ||
      grepl("converge",conditionMessage(w)))
    invokeRestart("muffleWarning")
}


setMethod("maxCPTParam","NeticaNode", function (node, Mstepit=5,
                                    tol=sqrt(.Machine$double.eps)) {
  ## Get the posterior pseudo-counts by multiplying each row of the
  ## node's CPT by its experience.
  np <- length(NodeParents(node))
  if (np==0L) {
    counts <- NodeProbs(node)*NodeExperience(node)
  } else {
    counts <- sweep(NodeProbs(node),1L:np,NodeExperience(node),"*")
  }
  withCallingHandlers(
      est <- mapDPC(counts,ParentStates(node),NodeStates(node),
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

MakePnode.NeticaNode <- function (net, name, data) {
  node <-PnetFindNode(net,name)
  if (is.null(node)) {
    node <- NewDiscreteNode(net,name,as.character(data$StateName))
  }
  node <- as.Pnode(node)
  if (nrow(data) != as.integer(data$Nstates[1]))
    stop("Must be one row in data for each state.")
  if (!is.null(data$NodeTitle))
    PnodeTitle(node) <- as.character(data$NodeTitle[1])
  if (!is.null(data$NodeDescription))
    PnodeDescription(node) <- as.character(data$NodeDescription[1])
  if (!is.null(data$NodeLabels)) {
    labels <- strsplit(data$NodeLabels[1],",")[[1]]
    PnodeLabels(node) <- as.character(labels)
  }

  PnodeStates(node) <- as.character(data$StateName)
  if (!is.null(data$StateTitle))
    PnodeStateTitles(node) <- as.character(data$StateTitle)
  if (!is.null(data$StateDescription))
    PnodeStateDescriptions(node) <- as.character(data$StateDescription)
  if (!is.null(data$StateValue) && !any(is.na(data$StateValue)))
    PnodeStateValues(node) <- as.character(data$StateValue)

  node

}
