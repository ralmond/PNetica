

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
