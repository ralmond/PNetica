### Netica specific implementations for the generics.

PnodeName.NeticaNode <- function (node)
  NodeName(node)

PnodeStates.NeticaNode <- function (node)
  NodeStates(node)

PnodeNumStates.NeticaNode <- function (node)
  NodeNumStates(node)

PnodeParents.NeticaNode <- function (node)
  NodeParents(node)

PnodeParentNames.NeticaNode <- function (node)
  sapply(NodeParents(node),NodeName)

PnodeNumParents.NeticaNode <- function (node)
  length(NodeParents(node))

