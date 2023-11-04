
## These layout functions work with igraph layouts.

setMethod("PnetIGLayout","NeticaBN",
          function (net, ig) {
  lay <- igraph::layout.auto(ig)
  vnames <- igraph::V(ig)$name
  for (v in 1:nrow(lay)) {
    vnode <- PnetFindNode(net,vnames[v])
    pos <- PnodeVisPos(vnode)
    if (all(pos!=0)) 
      lay[v,] <- pos
  }
  lay            
})

setMethod("PnetIGLayout<-","NeticaBN",
          function (net, ig, value) {
  vnames <- igraph::V(ig)$name
  for (v in 1:nrow(value)) {
    vnode <- PnetFindNode(net,vnames[v])
    PnodeVisPos(vnode) <- value[v,]
  }
  net                        
})         

