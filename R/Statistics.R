

setMethod("PnodeMargin",c("NeticaBN","NeticaNode"),
          function(net,node) NodeBeliefs(node))
setMethod("PnodeEAP",c("NeticaBN","NeticaNode"),
          function(net,node) c(NodeExpectedValue(node))) #clear SD attribute c()
setMethod("PnodeSD",c("NeticaBN","NeticaNode"),
          function(net,node) {
            ev <- NodeExpectedValue(node)
            if (is.na(ev)) return(NA)
            attr(ev,"std_dev")
            })
setMethod("PnodeMedian",c("NeticaBN","NeticaNode"),
          function(net,node)
            names(which(cumsum(NodeBeliefs(node))>=.5))[1])
setMethod("PnodeMode",c("NeticaBN","NeticaNode"),
          function(net,node)
            names(which.max(NodeBeliefs(node))))
