sess <- NeticaSession()
startSession(sess)

nodeman1 <- read.csv(system.file("auxdata", "Mini-PP-Nodes.csv", 
                     package="Peanut"),
                     row.names=1,stringsAsFactors=FALSE)

netman1 <- read.csv(system.file("auxdata", "Mini-PP-Nets.csv", 
                     package="Peanut"),
                    row.names=1, stringsAsFactors=FALSE)

### Test Net building
Nethouse <- BNWarehouse(manifest=netman1,session=sess,key="Name",
                        address=system.file("testnets",package="PNetica"))
stopifnot(is.PnetWarehouse(Nethouse))

CM <- WarehouseSupply(Nethouse,"miniPP_CM")
stopifnot(is.null(WarehouseFetch(Nethouse,"PPcompEM")))
EM1 <- WarehouseMake(Nethouse,"PPcompEM")

EMs <- lapply(c("PPcompEM","PPconjEM", "PPtwostepEM", "PPdurAttEM"),
              function(nm) WarehouseSupply(Nethouse,nm))


### Test Node Building with already loaded nets

Nodehouse <- NNWarehouse(manifest=nodeman1,
                         key=c("Model","NodeName"),
                         session=sess)
stopifnot(is.PnodeWarehouse(Nodehouse))

phyd <- WarehouseData(Nodehouse,c("miniPP_CM","Physics"))

p3 <- MakePnode.NeticaNode(CM,"Physics",phyd)

phys <- WarehouseSupply(Nodehouse,c("miniPP_CM","Physics"))
stopifnot(p3==phys)

for (n in 1:nrow(nodeman1)) {
  name <- as.character(nodeman1[n,c("Model","NodeName")])
  if (is.null(WarehouseFetch(Nodehouse,name))) {
    cat("Building Node ",paste(name,collapse="::"),"\n")
    WarehouseSupply(Nodehouse,name)
  }
}


SM <- WarehouseCopy(Nethouse,CM,"Student1")
stopifnot(PnetName(SM)=="Student1")
## Need to call Network All nodes to make sure nodes are copied.
stopifnot(setequal(names(NetworkAllNodes(CM)),
                   names(NetworkAllNodes(SM))))
          

SM1 <- WarehouseFetch(Nethouse,"Student1")
stopifnot(PnetName(SM1)=="Student1")



WarehouseFree(Nethouse,PnetName(EM1))
stopifnot(!is.valid(Nethouse,EM1))

