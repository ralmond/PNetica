### Netica specific implementations for the generics.

setMethod("PnodeName","NeticaNode", function (node) {
    nme <- NodeUserField(node,"Truename")
    if (is.null(nme) || is.na(nme)) {
        nme <- NodeName(node)
    }
    nme
})

setMethod("PnodeName<-","NeticaNode", function (node,value) {
  NodeUserField(node,"Truename") <- value
  NodeName(node) <- as.IDname(value)
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
    if (k < 1L) {
      bnds <- matrix(vals,0,2,
                     dimnames=list(character(),
                                   c("LowerBound","UpperBound")))
    } else {
      bnds <- matrix(c(vals[1L:k],vals[2L:(k+1L)]),k,2L,
                     dimnames=list(PnodeStates(node),
                                   c("LowerBound","UpperBound")))
    }
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
  mon1 <- isMonotonic(value[,1L])
  direct <- attr(mon1,"direction")
  mon2 <- isMonotonic(value[,2L])
  if (!mon1 || !mon2 || direct!=attr(mon2,"direction")) {
    stop ("State bounds for node",PnodeName(node),"are not monotonic.")
  }
  if (direct>0) {                       #Increasing
    if (!all(abs(value[2L:k,1L]-value[1L:(k-1L),2L])<.002)) {
      stop("Upper and lower bounds don't match for node ",PnodeName(node))
    }
    bnds <-c(value[1L:k,1L],value[k,2L])
  } else {                              #Decreasing
    if (!all(abs(value[1L:(k-1L),1L]-value[2L:k,2L])<.002)) {
      stop("Upper and lower bounds don't match for node ",PnodeName(node))
    }
    bnds <-c(value[1L,2L],value[1L:k,1L])
  }
  NodeLevels(node) <- bnds
  if (!is.null(rownames(value)) &&
      (length(PnodeStates(node)!=k) || all(nchar(PnodeStates(node))==0L))) {
    PnodeStates(node) <- rownames(value)
  }
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
    pnames <- sapply(parents,PnodeName)
    stubsp <- sapply(parents,function(nd) NodeKind(nd)=="Stub")
    if (any(stubsp))
      pnames[stubsp] <- names(parents)[stubsp]
    pnames
  }
})

setMethod("PnodeNumParents","NeticaNode", function (node)
  length(NodeParents(node)))


setMethod("PnodeEvidence","NeticaNode",
          function(node) {
            if (is.continuous(node)) {
              res <- NodeValue(node)
            } else {
              res <- NodeFinding(node)
              if (res=="@NO FINDING") {
                res <- NA
              } else if (res=="@LIKELIHOOD") {
                res <- NodeLikelihood(node)
              } else if (res=="@NEGATIVE FINDINGS") {
                res <- NodeLikelihood(node)
              }
            }
            res
          })


setMethod("PnodeEvidence<-",c("NeticaNode","numeric"),
          function (node,value) {
            if (length(value) == 1L) {
              NodeValue(node) <- value
            } else if (length(value)==PnodeNumStates(node)) {
              NodeLikelihood(node) <- value
            }
            invisible(node)
          })

setOldClass("difftime")
setMethod("PnodeEvidence<-",c("NeticaNode","difftime"),
          function (node,value) {
            units(value) <- "secs"
            NodeValue(node) <- as.numeric(value)
          })
setMethod("PnodeEvidence<-",c("NeticaNode","character"),
          function (node,value) {
            ov1 <- value
            sts <- NodeStates(node)
            if (!(ov1 %in% sts)) {
              ov1 <- sts[toupper(sts)==toupper(ov1)]
            }
            if (length(ov1) > 0L) {
              flog.trace("Setting observable %s to %s.",NodeName(node),ov1)
              NodeFinding(node) <- ov1
            } else {
              flog.warn("Observable %s has unknown value %s, skipping.",
                        NodeName(node), ov1)
            }
            invisible(node)
          })

setMethod("PnodeEvidence<-",c("NeticaNode","factor"),
          function (node,value) {
            PnodeEvidence(node) <- as.character(value)
            })
setMethod("PnodeEvidence<-",c("NeticaNode","NULL"),
          function (node,value) {
            RetractNodeFinding(node)
            })
setMethod("PnodeEvidence<-",c("NeticaNode","logical"),
          function (node,value) {
            levs <- NodeLevels(node)
            if (length(levs) != 2L) {
              PnodeEvidence(node) <- as.character(value)
            } else {
              v1 <- names(levs)[levs==as.numeric(value)]
              if (length(v1) != 1L) {
                stop("When setting ",NodeName(node)," expected ",names(levs),
                     " got ",value)
              }
              PnodeEvidence(node) <- v1
            }
            node
          })

setMethod("PnodeEvidence<-",c("NeticaNode","ANY"),
          function(node,value) {
            warning("Don't know how to set ",NodeName(node)," to ",
                 as.character(value),".  Skipping.")
          })



##############################################################
#### Net Functions

setMethod("PnetName","NeticaBN", function (net){
    nme <- NetworkUserField(net,"Truename")
    if (is.null(nme) || is.na(nme)) {
        nme <- NetworkName(net)
    }
    nme
})

setMethod("PnetName<-","NeticaBN", function (net, value) {
  NetworkUserField(net,"Truename")
  NetworkName(net) <- as.IDname(value)
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


setMethod("PnetFindNode","NeticaBN", function(net,name) {
  NetworkFindNode(net,as.IDname(name))
})

setMethod("PnetSerialize","NeticaBN",
          function (net) {
            factory <- net$Session$SessionName
            name <- PnetName(net)
            tmpfile <- file.path(tempdir(),paste(name,"dne",sep="."))
            if (file.exists(tmpfile)) file.remove(tmpfile)
            WriteNetworks(net,tmpfile)
            data <- serialize(readLines(tmpfile),NULL)
            list(name=name,factory=factory,data=data)
          })


setMethod("unserializePnet","NeticaSession",
          function(factory,data) {
            name <- data$name
            tmpfile <- file.path(tempdir(),paste(name,"dne",sep="."))
            if (file.exists(tmpfile)) file.remove(tmpfile)
            writeLines(unserialize(data$data),tmpfile)
            oldnet <- factory$findNet(name)
            if (!is.null(oldnet) && is.active(oldnet)) {
              flog.warn("Replacing old version of network %s.",
                           name)
              DeleteNetwork(oldnet)
            }
            ReadNetworks(tmpfile,factory)
          })

setMethod("PnetCompile","NeticaBN",function(net) CompileNetwork(net))
