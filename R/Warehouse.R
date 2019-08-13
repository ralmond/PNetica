BNWarehouse <- setClass("BNWarehouse",
                       slots=c(manifest="data.frame",
                                session="NeticaSession",
                                key="character")
                       )
setMethod(ClearWarehouse,"BNWarehouse",
          function(warehouse) {
            warning("To clear warehouse, stop and restart session.")
          })
setMethod(WarehouseManifest,"BNWarehouse",
          function(warehouse) {warehouse@manifest})
setMethod("WarehouseManifest<-",c("BNWarehouse","data.frame"),
          function(warehouse,value) {
            for (ky in key) {
              value[[ky]] <- trimws(value[[ky]])
            }
            warehouse@manifest<-value; warehouse})

setMethod(WarehouseData,"BNWarehouse",
          function(warehouse,name) {
            key <- warehouse@key
            if (length(name) != length(key))
              stop("Expected name to contain elements",key)
            manifest <- warehouse@manifest
            whch = rep(TRUE,nrow(manifest))
            for (i in 1:length(key)) {
              whch <- whch & manifest[[key[i]]] == name[i]
            }
            manifest[whch,,drop=FALSE]
          })

setMethod(WarehouseFetch,"BNWarehouse",
          function(warehouse,name) {
            if (length(name) != 1L)
              stop("Expected name ",name," to be unique.")
            warehouse@session$nets[[name]]
          })

setMethod("WarehouseSupply", c("ANY"), function(warehouse,name) {
  val <- WarehouseFetch(warehouse,name)
  if (is.null(val))
    val <- WarehouseMake(warehouse,name)
  if (!is.active(val)) {
    warehouse@session$nets[[name]] <- NULL
    val <- WarehouseMake(warehouse,name)
  }
  val
})


setMethod(WarehouseMake,"BNWarehouse",
          function(warehouse,name) {
            if (length(name) != 1L)
              stop("Expected name to be unique.")
            sess <- warehouse@session
            if (!is.null(sess$nets[[name]])) {
              warning("Deleting old network ",name)
              DeleteNetwork(sess$nets[[name]])
            }
            dat <- WarehouseData(warehouse,name)
            MakePnet.NeticaBN(sess,name,dat)
          })


setMethod(WarehouseFree,"BNWarehouse",
          function(warehouse,name) {
            warning("To free network, call DeleteNetworks.")
          })

setMethod(WarehouseInventory,"BNWarehouse",
          function(warehouse) {
            allKeys <- warehouse@manifest[,warehous@key,drop=FALSE]
            built <- sapply(1L:nrow(allkeys),
                            function (k)
                              !is.null(WarehouseFetch(warehouse,allKeys[k,]))
                            )
            allkeys[built, ,drop=FALSE]})

setMethod(is.PnetWarehouse,"BNWarehouse",
          function(obj) {TRUE})

setMethod("WarehouseUnpack", "BNWarehouse",
          function(warehouse,serial) {
            unserializePnet(warehouse@session,serial)
            warehouse@session$nets[[serial$name]]
          })



#######  Node Warehouse

NNWarehouse <- setClass("NNWarehouse",
                       slots=c(manifest="data.frame",
                               session="NeticaSession",
                               key="character")
                       )


setMethod(ClearWarehouse,"NNWarehouse",
          function(warehouse) {
            warning("To clear warehouse, delete and recreate network.")
          })


setMethod(WarehouseManifest,"NNWarehouse",
          function(warehouse) {warehouse@manifest})
setMethod("WarehouseManifest<-",c("NNWarehouse","data.frame"),
          function(warehouse,value) {
            for (ky in key) {
              value[[ky]] <- trimws(value[[ky]])
            }
            warehouse@manifest<-value;
            warehouse})

setMethod(WarehouseData,"NNWarehouse",
          function(warehouse,name) {
            key <- warehouse@key
            if (length(name) != length(key))
              stop("Expected name to contain elements",key)
            manifest <- warehouse@manifest
            whch = rep(TRUE,nrow(manifest))
            for (i in 1:length(key)) {
              whch <- whch & manifest[[key[i]]] == name[i]
            }
            manifest[whch,,drop=FALSE]
          })

setMethod(WarehouseFetch,"NNWarehouse",
          function(warehouse,name) {
            if (length(name) != 2L)
              stop("Expected key to look like (net, node).")
            sess <- warehouse@session
            sess$nets[[name[1]]]$nodes[[name[2]]]
          })

setMethod(WarehouseMake,"NNWarehouse",
          function(warehouse,name) {
            if (length(name) != 2L)
              stop("Expected name to be of the form (net,node).")
            net <- warehouse@session$nets[[name[1]]]
            if (is.null(net)) {
              stop("Network ",name[1]," does not exist.")
            }
            if (!is.null(net$nodes[[name[2]]])) {
              warning("Deleting old node ",paste(name,collapse="::"))
              DeleteNodes(net$nodes[[name[2]]])
            }
            dat <- WarehouseData(warehouse,name)
            MakePnode.NeticaNode(net,name[2],dat)
          })


setMethod(WarehouseFree,"NNWarehouse",
          function(warehouse,name) {
            warning("Delete the node to free it.")
          })

setMethod(is.PnodeWarehouse,"NNWarehouse",
          function(obj) {TRUE})

setMethod(WarehouseInventory,"NNWarehouse",
          function(warehouse) {
            allKeys <- warehouse@manifest[,warehous@key,drop=FALSE]
            built <- sapply(1L:nrow(allkeys),
                            function (k)
                              !is.null(WarehouseFetch(warehouse,allKeys[k,]))
                            )
            allkeys[built, ,drop=FALSE]})
