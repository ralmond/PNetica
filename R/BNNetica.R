### Netica specific implementations for the generics.

setMethod("PnodeName","NeticaNode", function (node)
  NodeName(node)
  )

setMethod("PnodeName<-","NeticaNode", function (node,value) {
  NodeName(node) <- value
  invisible(node)
})


setMethod("PnodeTitle","NeticaNode", function (node)
  NodeTitle(node))

setMethod("PnodeTitle<-","NeticaNode", function (node,value) {
  NodeTitle(node) <- value
  invisible(node)
})

setMethod("PnodeDescription","NeticaNode", function (node)
  NodeDescription(node))

setMethod("PnodeDescription<-","NeticaNode", function (node,value) {
  NodeDescription(node) <- value
  invisible(node)
})

setMethod("PnodeLabels","NeticaNode", function (node)
  NodeSets(node))

setMethod("PnodeLabels<-","NeticaNode", function (node,value) {
  NodeSets(node) <- value
  invisible(node)
})


### States

setMethod("PnodeStates","NeticaNode", function (node)
  NodeStates(node))

setMethod("PnodeStates<-","NeticaNode", function (node,value) {
  ## Not 100% sure if this is safe, but want to simplify
  ## Interface.
  NodeStates(node,resize=TRUE) <- value
  invisible(node)
})

setMethod("PnodeNumStates","NeticaNode", function (node)
  NodeNumStates(node))

setMethod("PnodeStateTitles","NeticaNode", function (node) {
  NodeStateTitles(node)
})

setMethod("PnodeStateTitles<-","NeticaNode", function (node,value) {
  NodeStateTitles(node) <- value
  invisible(node)
})


setMethod("PnodeStateDescriptions","NeticaNode", function (node) {
  NodeStateComments(node)
})

setMethod("PnodeStateDescriptions<-","NeticaNode", function (node,value) {
  NodeStateComments(node) <- value
  invisible(node)
})

setMethod("PnodeStateValues","NeticaNode", function (node)
  if (is.continuous(node)) {
    apply(PnodeStateBounds(node),1,median)
  } else {
    NodeLevels(node)
  })

setMethod("PnodeStateValues<-","NeticaNode", function (node,value) {
  if (is.continuous(node))
    stop("This function only available for discrete nodes, but ",
         PnodeName(node), " is continuous. Use PnodeStateBounds instead.")
  NodeLevels(node) <- value
  invisible(node)
})

setMethod("PnodeStateBounds","NeticaNode", function (node)
  if (is.continuous(node)) {
    vals <- NodeLevels(node)
    k <- length(vals) -1L
    bnds <- matrix(c(vals[1L:k],vals[2L:(k+1L)]),k,2L,
                   dimnames=list(PnodeStates(node),
                                 c("LowerBound","UpperBound")))

    bnds
  } else {
    stop("This function only available for continuous nodes, but ",
         PnodeName(node), " is discrete. Use PnodeStateValues instead.")
  })

setMethod("PnodeStateBounds<-","NeticaNode", function (node,value) {
  if (!is.continuous(node))
    stop("This function only available for continuous nodes, but ",
         PnodeName(node), " is discrete. Use PnodeStateValues instead.")
  k <- nrow(value)
  if (!all(abs(value[2L:k,1L]-value[1L:(k-1L),2L])<.0001)) {
    stop("Upper and lower bounds don't match for node ",PnodeName(node))
  }
  bnds <-c(value[1L:k,1L],value[k,2L])
  NodeLevels(node) <- bnds
  invisible(node)
})

setMethod("isPnodeContinuous","NeticaNode", function (node)
  is.continuous(node))

#### Parents



setMethod("PnodeParents","NeticaNode", function (node)
  NodeParents(node)
)
setMethod("PnodeParents<-","NeticaNode", function (node,value) {
  if (is.null(value)) value <- list()
  NodeParents(node) <- value
  invisible(node)
})


### Parents

setMethod("PnodeParentNames","NeticaNode", function (node) {
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
})

setMethod("PnodeNumParents","NeticaNode", function (node)
  length(NodeParents(node)))


#### Net Functions

setMethod("PnetName","NeticaBN", function (net){
  NetworkName(net)
})

setMethod("PnetName<-","NeticaBN", function (net, value) {
  NetworkName(net) <- value
  invisible(net)
})


setMethod("PnetTitle","NeticaBN", function (net) {
  NetworkTitle(net)
})


setMethod("PnetTitle<-","NeticaBN", function (net, value) {
  NetworkTitle(net) <- value
  invisible(net)
})


## The HUB is the name of the CM for an EM, or "" for an CM.
setMethod("PnetHub","NeticaBN", function (net) {
  NetworkUserField(net,"Hub")
})


## Value could be the actual model or its name.
setMethod("PnetHub<-","NeticaBN", function (net, value) {
  NetworkUserField(net,"Hub") <-value
  invisible(net)
})

## Note:  This is not necessarily the same as the GetNeticaPathname() function.
setMethod("PnetPathname","NeticaBN", function (net) {
  value <- NetworkUserField(net,"Pathname")
  if (is.na(value) || is.null(value) || nchar(value)==0L) {
    value <- attr(net,"Filename")
  }
  value
})

setMethod("PnetPathname<-","NeticaBN", function (net, value) {
  NetworkUserField(net,"Pathname") <-value
  invisible(net)
})

setMethod("PnetDescription","NeticaBN", function (net) {
  NetworkComment(net)
})

setMethod("PnetDescription<-","NeticaBN", function (net, value) {
  NetworkComment(net) <- value
  invisible(net)
})


setMethod("PnetFindNode","NeticaBN", function(net,name)
  NetworkFindNode(net,name))


setMethod("PnetSerialize","NeticaBN",
          function (net) {
            factory <- net$Session$SessionName
            name <- PnetName(net)
            tmpfile <- file.path(tempdir(),paste(name,"dne",sep="."))
            WriteNetworks(net,tmpfile)
            data <- serialize(readLines(tmpfile),NULL)
            list(name=name,factory=factory,data=data)
          })


setMethod("unserializePnet","NeticaSession",
          function(factory,data) {
            name <- data$name
            tmpfile <- file.path(tempdir(),paste(name,"dne",sep="."))
            writeLines(unserialize(data$data),tmpfile)
            oldnet <- factory$findNet(name)
            if (!is.null(oldnet) && is.active(oldnet)) {
              DeleteNetwork(oldnet)
            }
            ReadNetworks(tmpfile,factory)
          })

