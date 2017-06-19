### Netica specific implementations for the generics.

PnodeName.NeticaNode <- function (node)
  NodeName(node)

"PnodeName<-.NeticaNode" <- function (node,value) {
  NodeName(node) <- value
  invisible(node)
}


PnodeTitle.NeticaNode <- function (node)
  NodeTitle(node)

"PnodeTitle<-.NeticaNode" <- function (node,value) {
  NodeTitle(node) <- value
  invisible(node)
}

PnodeDescription.NeticaNode <- function (node)
  NodeDescription(node)

"PnodeDescription<-.NeticaNode" <- function (node,value) {
  NodeDescription(node) <- value
  invisible(node)
}

PnodeLabels.NeticaNode <- function (node)
  NodeSets(node)

"PnodeLabels<-.NeticaNode" <- function (node,value) {
  NodeSets(node) <- value
  invisible(node)
}


### States

PnodeStates.NeticaNode <- function (node)
  NodeStates(node)

"PnodeStates<-.NeticaNode" <- function (node,value) {
  ## Not 100% sure if this is safe, but want to simplify
  ## Interface.
  NodeStates(node,resize=TRUE) <- value
  invisible(node)
}

PnodeNumStates.NeticaNode <- function (node)
  NodeNumStates(node)

PnodeStateTitles.NeticaNode <- function (node) {
  NodeStateTitles(node)
}

"PnodeStateTitles<-.NeticaNode" <- function (node,value) {
  NodeStateTitles(node) <- value
  invisible(node)
}


PnodeStateDescriptions.NeticaNode <- function (node) {
  NodeStateComments(node)
}

"PnodeStateDescriptions<-.NeticaNode" <- function (node,value) {
  NodeStateComments(node) <- value
  invisible(node)
}

PnodeStateValues.NeticaNode <- function (node)
  NodeLevels(node)


"PnodeStateValues<-.NeticaNode" <- function (node,value) {
  NodeLevels(node) <- value
  invisible(node)
}


#### Parents



PnodeParents.NeticaNode <- function (node)
  NodeParents(node)

"PnodeParents<-.NeticaNode" <- function (node,value) {
  NodeParents(node) <- value
}


### Parents

PnodeParentNames.NeticaNode <- function (node) {
  if (PnodeNumParents(node)==0) {
    character()
  } else {
    parents <- NodeParents(node)
    pnames <- sapply(parents,NodeName)
    stubsp <- sapply(parents,function(nd) NodeKind(nd)=="Stub")
    if (any(stubsp))
      pnames[stubsp] <- names(parents)[stubsp]
    pnames
  }
}

PnodeNumParents.NeticaNode <- function (node)
  length(NodeParents(node))


#### Net Functions

PnetName.NeticaBN <- function (net){
  NetworkName(net)
}

"PnetName<-.NeticaBN" <- function (net, value) {
  NetworkName(net) <- value
  invisible(net)
}


PnetTitle.NeticaBN <- function (net) {
  NetworkTitle(net)
}


"PnetTitle<-.NeticaBN" <- function (net, value) {
  NetworkTitle(net) <- value
  invisible(net)
}


## The HUB is the name of the CM for an EM, or "" for an CM.
PnetHub.NeticaBN <- function (net) {
  NetworkUserField(net,"Hub")
}


## Value could be the actual model or its name.
"PnetHub<-.NeticaBN" <- function (net, value) {
  NetworkUserField(net,"Hub") <-value
  invisible(net)
}

## Note:  This is not necessarily the same as the GetNeticaPathname() function.
PnetPathname.NeticaBN <- function (net) {
  value <- NetworkUserField(net,"Pathname")
  if (is.na(value) || is.null(value) || nchar(value)==0L) {
    value <- attr(net,"Filename")
  }
  value
}

"PnetPathname<-.NeticaBN" <- function (net, value) {
  NetworkUserField(net,"Pathname") <-value
  invisible(net)
}

PnetDescription.NeticaBN <- function (net) {
  NetworkComment(net)
}

"PnetDescription<-.NeticaBN" <- function (net, value) {
  NetworkComment(net) <- value
  invisible(net)
}


PnetFindNode.NeticaBN <- function(net,name)
  NetworkFindNode(net,name)

