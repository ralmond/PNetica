BNWarehouse <- setClass("BNWarehouse",
                       slots=c(manifest="data.frame",
                               session="NeticaSession",
                               address="character",
                               key="character",
                               prefix="character")
                       )
BNWarehouse <- function(manifest=data.frame(),session=getDefaultSession(),
                        address=".",key=c("Name"),prefix="S")
  new("BNWarehouse",manifest=manifest, session=session, address=address,
      key=key, prefix=prefix)

setIs("BNWarehouse","PnetWarehouse")

setMethod(ClearWarehouse,"BNWarehouse",
          function(warehouse) {
            objs <- objects(warehouse@session$nets)
            for (obj in objs) {
              net <- warehouse@session$nets[[obj]]
              if (is.NeticaBN(net) && is.active(net)) {
                flog.trace("Clearing Network %s",obj)
                DeleteNetwork(net)
              }
            }
          })

setMethod(WarehouseManifest,"BNWarehouse",
          function(warehouse) {warehouse@manifest})
setMethod("WarehouseManifest<-",c("BNWarehouse","data.frame"),
          function(warehouse,value) {
            for (ky in warehouse@key) {
              value[[ky]] <- trimws(value[[ky]])
            }
            warehouse@manifest<- value; warehouse})


setGeneric("WarehouseDirectory",
           function (warehouse) standardGeneric("WarehouseDirectory"))
setMethod("WarehouseDirectory","BNWarehouse",
          function (warehouse) {
            warehouse@address
          })
setGeneric("WarehouseDirectory<-",
           function (warehouse, value) standardGeneric("WarehouseDirectory<-"))
setMethod("WarehouseDirectory<-","BNWarehouse",
          function (warehouse,value) {
            warehouse@address <- value
            warehouse
          })



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
            dat <- manifest[whch,,drop=FALSE]
            ## Add directory information to pathnames.
            dir <- do.call("file.path",as.list(warehouse@address))
            if (length(dir) > 0L)
              dat$Pathname <- file.path(dir,dat$Pathname)
            dat
          })

setMethod(WarehouseFetch,"BNWarehouse",
          function(warehouse,name) {
            if (length(name) != 1L)
              stop("Expected name ",name," to be unique.")
            warehouse@session$nets[[as.IDname(name)]]
          })

setMethod("WarehouseSupply", c("BNWarehouse"), function(warehouse,name) {
  val <- WarehouseFetch(warehouse,name)
  if (is.null(val))
    val <- WarehouseMake(warehouse,name)
  if (!is.active(val)) {
    warehouse@session$nets[[as.IDname(name)]] <- NULL
    val <- WarehouseMake(warehouse,name)
  }
  val
})


setMethod(WarehouseMake,"BNWarehouse",
          function(warehouse,name) {
            if (length(name) != 1L)
              stop("Expected name to be unique.")
            dat <- WarehouseData(warehouse,name)
            if (nrow(dat) <1L)
              stop("Cannot find manifest data for network ",name)
            if (nrow(dat) >2L)
              warning("Multiple manifest data row for network ",name)
            sess <- warehouse@session
            if (!is.null(sess$nets[[as.IDname(name)]])) {
              warning("Deleting old network ",name)
              DeleteNetwork(sess$nets[[as.IDname(name)]])
            }
            MakePnet.NeticaBN(sess,name,dat)
          })

setMethod(WarehouseSave,c("BNWarehouse","character"),
          function(warehouse,obj) {
            net <- warehouse@session$nets[[as.IDname(obj)]]
            if (is.null(net)) {
              warning("Network named ",obj," does not exist, not saving.")
            } else {
              WarehouseSave(warehouse,net)
            }
          })

setMethod(WarehouseSave,c("BNWarehouse","NeticaBN"),
          function(warehouse,obj) {
            name <- PnetName(obj)
            pname <- PnetPathname(obj)
            WriteNetworks(obj,pname)
          })


setMethod(WarehouseFree,"BNWarehouse",
          function(warehouse,name) {
            net <- WarehouseFetch(warehouse,name)
            if (is.null(net)) {
              flog.trace("Network for name %s not found, skipping.",name)
            } else {
              if (is.active(net))
                DeleteNetwork(net)
              if (!is.null(warehouse@session$nets[[name]]))
                rm(list=name,envir=warehouse@session$nets)
            }
          })

setMethod(WarehouseCopy,c("BNWarehouse","NeticaBN"),
          function(warehouse,obj,newname) {
            newname <- as.legal.name(warehouse,newname)
            CopyNetworks(obj,newname)
          })

setMethod(is.legal.name,"BNWarehouse",
          function(warehouse,name)
            is.IDname(name)
          )

setMethod(as.legal.name,"BNWarehouse",
          function(warehouse,name)
            as.IDname(name,warehouse@prefix)
          )

setMethod(is.valid,"BNWarehouse",
          function(warehouse,object)
            is.active(object)
          )


setMethod(WarehouseInventory,"BNWarehouse",
          function(warehouse) {
            allKeys <- warehouse@manifest[,warehouse@key,drop=FALSE]
            built <- sapply(1L:nrow(allKeys),
                            function (k)
                              !is.null(WarehouseFetch(warehouse,allKeys[k,]))
                            )
            allKeys[built, ,drop=FALSE]})

setMethod(is.PnetWarehouse,"BNWarehouse",
          function(obj) {TRUE})

setMethod("WarehouseUnpack", "BNWarehouse",
          function(warehouse,serial) {
            unserializePnet(warehouse@session,serial)
            warehouse@session$nets[[as.IDname(serial$name)]]
          })



#######  Node Warehouse

NNWarehouse <- setClass("NNWarehouse",
                       slots=c(manifest="data.frame",
                               session="NeticaSession",
                               key="character",
                               prefix="character")
                       )
NNWarehouse <- function(manifest=data.frame(),session=getDefaultSession(),
                        key=c("Model","NodeName"),prefix="V")
  new("NNWarehouse",manifest=manifest, session=session,
      key=key, prefix=prefix)

setIs("NNWarehouse","PnodeWarehouse")

setMethod(ClearWarehouse,"NNWarehouse",
          function(warehouse) {
            warning("To clear warehouse, delete and recreate network.")
          })


setMethod(WarehouseManifest,"NNWarehouse",
          function(warehouse) {warehouse@manifest})
setMethod("WarehouseManifest<-",c("NNWarehouse","data.frame"),
          function(warehouse,value) {
            for (ky in warehouse@key) {
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
            sess$nets[[as.IDname(name[1])]]$nodes[[as.IDname(name[2])]]
          })

setMethod(WarehouseMake,"NNWarehouse",
          function(warehouse,name) {
            if (length(name) != 2L)
              stop("Expected name to be of the form (net,node).")
            net <- warehouse@session$nets[[as.IDname(name[1])]]
            if (is.null(net)) {
              stop("Network ",name[1]," does not exist.")
            }
            if (!is.null(net$nodes[[as.IDname(name[2])]])) {
              warning("Deleting old node ",paste(name,collapse="::"))
              DeleteNodes(net$nodes[[as.IDname(name[2])]])
            }
            dat <- WarehouseData(warehouse,name)
            MakePnode.NeticaNode(net,name[2],dat)
          })


setMethod(WarehouseFree,"NNWarehouse",
          function(warehouse,name) {
            node <- WarehouseFetch(warehouse,name)
            if (is.null(node)) {
              flog.trace("Node for name %s not found, skipping.",name)
            } else {
              if (is.active(node))
                DeleteNodes(node)
            }
          })

setMethod(WarehouseSave,"NNWarehouse",
          function(warehouse,obj) {})   #Null Action.


setMethod(WarehouseCopy,c("NNWarehouse","NeticaNode"),
          function(warehouse,obj,newname) {
            newname <- as.legal.name(warehouse,newname)
            if (length(newname) != 2L)
              stop("Expected key to look like (net, node).")
            newnet <- warehouse@session$nets[[newname[1]]]
            if (is.null(newnet))
              stop("Network ",newname[1]," does not exist.")
            CopyNodes(obj,newname[2],newnet=newnet)
          })

setMethod(is.legal.name,"NNWarehouse",
          function(warehouse,name)
            is.IDname(name)
          )

setMethod(as.legal.name,"NNWarehouse",
          function(warehouse,name)
            as.IDname(name,warehouse@prefix)
          )

setMethod(is.valid,"NNWarehouse",
          function(warehouse,object)
            is.active(object)
          )

setMethod(is.PnodeWarehouse,"NNWarehouse",
          function(obj) {TRUE})

setMethod(WarehouseInventory,"NNWarehouse",
          function(warehouse) {
            allKeys <- warehouse@manifest[,warehouse@key,drop=FALSE]
            built <- sapply(1L:nrow(allKeys),
                            function (k)
                              !is.null(WarehouseFetch(warehouse,allKeys[k,]))
                            )
            allKeys[built, ,drop=FALSE]})
setMethod("WarehouseSupply", c("NNWarehouse"), function(warehouse,name) {
  val <- WarehouseFetch(warehouse,name)
  if (is.null(val))
    val <- WarehouseMake(warehouse,name)
  if (!is.active(val)) {
    warehouse@session$nets[[as.IDname(name[1])]]$nodes[[as.IDname(name[2])]] <- NULL
    val <- WarehouseMake(warehouse,name)
  }
  val
})


