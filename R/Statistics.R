##'Statistic methods for \code{"NeticaBN"} class.
##'
##'
##'These are the implementation for the basic statistic calculation methods.
##'
##'
##'@name Statistic.NeticaNode
##'@aliases Statistic.NeticaNode PnodeEAP,NeticaBN,NeticaNode-method
##'PnodeEAP,NeticaBN,character-method PnodeSD,NeticaBN,NeticaNode-method
##'PnodeSD,NeticaBN,character-method PnodeMargin,NeticaBN,NeticaNode-method
##'PnodeMargin,NeticaBN,character-method PnodeMedian,NeticaBN,NeticaNode-method
##'PnodeMedian,NeticaBN,character-method PnodeMode,NeticaBN,NeticaNode-method
##'PnodeMode,NeticaBN,character-method
##'@docType class
##'@section Methods:
##'
##'All methods have signature \code{signature(net = "NeticaBN", node =
##'"NeticaNode")} and \code{signature(net = "NeticaBN", node = "character")}.
##'The later form is more often used, and takes the name of the node and finds
##'the appropriate node in the network.
##'
##'\describe{ \item{list("PnodeEAP")}{Calculates the marginal distribution of
##'the node. Statistic returns a named vector of values.}
##'\item{list("PnodeEAP")}{Calculates the expected value of the node; assumes
##'numeric values have been set with \code{\link[Peanut]{PnodeStateValues}}. }
##'\item{list("PnodeSD")}{Calculates the standard deviation of the node;
##'assumes numeric values have been set with
##'\code{\link[Peanut]{PnodeStateValues}}. }
##'\item{list("PnodeMedian")}{Calculates the median state (state whose
##'cumulative probability covers .5) of the node. Statistic returns the name of
##'the state.} \item{list("PnodeMode")}{Calculates the modal (most likely)
##'state of the node. Statistic returns the name of the state.} }
##'@author Russell Almond
##'@seealso Statistics Class: \code{\linkS4class{Statistic}}
##'
##'Constructor function: \code{\link{Statistic}}
##'
##'\code{\link{calcStat}}
##'
##'These statistics will likely produce errors unless \code{\link{PnetCompile}}
##'has been run first.
##'@references Almond, R.G., Mislevy, R.J. Steinberg, L.S., Yan, D. and
##'Willamson, D.  M. (2015). \emph{Bayesian Networks in Educational
##'Assessment}.  Springer.  Chapter 13.
##'@keywords graphs
##'@examples
##'
##'
##'sess <- NeticaSession()
##'startSession(sess)
##'
##'irt10.base <- ReadNetworks(paste(library(help="PNetica")$path,
##'                           "testnets","IRT10.2PL.base.dne",
##'                           sep=.Platform$file.sep),session=sess)
##'irt10.base <- as.Pnet(irt10.base)  ## Flag as Pnet, fields already set.
##'irt10.theta <- PnetFindNode(irt10.base,"theta")
##'irt10.items <- PnetPnodes(irt10.base)
##'## Flag items as Pnodes
##'for (i in 1:length(irt10.items)) {
##'  irt10.items[[i]] <- as.Pnode(irt10.items[[i]])
##'  
##'}
##'## Make some statistics
##'marginTheta <- Statistic("PnodeMargin","theta","Pr(theta)")
##'meanTheta <- Statistic("PnodeEAP","theta","EAP(theta)")
##'sdTheta <- Statistic("PnodeSD","theta","SD(theta)")
##'medianTheta <- Statistic("PnodeMedian","theta","Median(theta)")
##'modeTheta <- Statistic("PnodeMedian","theta","Mode(theta)")
##'
##'
##'BuildAllTables(irt10.base)
##'PnetCompile(irt10.base) ## Netica requirement
##'
##'calcStat(marginTheta,irt10.base)
##'calcStat(meanTheta,irt10.base)
##'calcStat(sdTheta,irt10.base)
##'calcStat(medianTheta,irt10.base)
##'calcStat(modeTheta,irt10.base)
##'
##'DeleteNetwork(irt10.base)
##'stopSession(sess)
##'
##'
NULL

setMethod("PnodeMargin",c("NeticaBN","NeticaNode"),
          function(net,node) NodeBeliefs(node))
setMethod("PnodeMargin",c("NeticaBN","character"),
          function(net,node) {
            nd <- NetworkFindNode(net,node)
            if (!is.NeticaNode(nd))
              stop("Can't find node named ",node)
            PnodeMargin(net,nd)
          })
setMethod("PnodeEAP",c("NeticaBN","NeticaNode"),
          function(net,node) c(NodeExpectedValue(node))) #clear SD attribute c()
setMethod("PnodeEAP",c("NeticaBN","character"),
          function(net,node) {
            nd <- NetworkFindNode(net,node)
            if (!is.NeticaNode(nd))
              stop("Can't find node named ",node)
            PnodeEAP(net,nd)
          })
setMethod("PnodeSD",c("NeticaBN","NeticaNode"),
          function(net,node) {
            ev <- NodeExpectedValue(node)
            if (is.na(ev)) return(NA)
            attr(ev,"std_dev")
            })
setMethod("PnodeSD",c("NeticaBN","character"),
          function(net,node) {
            nd <- NetworkFindNode(net,node)
            if (!is.NeticaNode(nd))
              stop("Can't find node named ",node)
            PnodeSD(net,nd)
          })
setMethod("PnodeMedian",c("NeticaBN","NeticaNode"),
          function(net,node)
            names(which(cumsum(NodeBeliefs(node))>=.5))[1])
setMethod("PnodeMedian",c("NeticaBN","character"),
          function(net,node) {
            nd <- NetworkFindNode(net,node)
            if (!is.NeticaNode(nd))
              stop("Can't find node named ",node)
            PnodeMedian(net,nd)
          })
setMethod("PnodeMode",c("NeticaBN","NeticaNode"),
          function(net,node)
            names(which.max(NodeBeliefs(node))))
setMethod("PnodeMode",c("NeticaBN","character"),
          function(net,node) {
            nd <- NetworkFindNode(net,node)
            if (!is.NeticaNode(nd))
              stop("Can't find node named ",node)
            PnodeMode(net,nd)
          })
