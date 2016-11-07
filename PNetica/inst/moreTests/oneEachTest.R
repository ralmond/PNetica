
## Network with two proficiency variables and observables for each
## different type of rule
library(PNetica)
binAll <- CreateNetwork("binAll") # creates network in Netica
PnetPriorWeight(binAll) <- 11           #Give it something to see.

## Set up Proficiency Model. Creates theta1 and theta0 with binary levels
thetas <- NewDiscreteNode(binAll,paste("theta",0:1,sep=""),
                          c("Low","Med","High")) # Create the variable with 3 levels
names(thetas) <- paste("theta",0:1,sep="")

NodeParents(thetas[[2]]) <- thetas[1] #sets the parent of theta1 as theta0

for (nd in thetas) {
  NodeLevels(nd) <- effectiveThetas(NodeNumStates(nd))
  PnodeRules(nd) <- "Compensatory"
  PnodeLink(nd) <- "normalLink"
  PnodeQ(nd) <- TRUE
  PnodeBetas(nd) <- 0 # A numeric vector of intercept parameters
  NodeSets(nd) <- c("pnodes","Proficiency") # A character vector containing the names of the node sets
}

## Standard normal prior.
PnodeAlphas(thetas[[1]]) <- numeric() # A numeric vector of (log) slope parameters
PnodeLinkScale(thetas[[1]]) <- 1 # A positive numeric value, or NULL if the scale parameter is not used for the link function.
## Regression with a correlation of .6
PnodeAlphas(thetas[[2]]) <- .6
PnodeLinkScale(thetas[[2]]) <- .8

### Now Create Observables
obsnames <- c("t0","t1","t2","t3","t4","comp","conj","disj","oconj","odis") #items
obs <- NewDiscreteNode(binAll,obsnames)
names(obs) <- obsnames

for (nd in obs) {
  NodeParents(nd) <- thetas
  NodeLevels(nd) <- c(1,0) #provides two levels for each item
  PnodeLink(nd) <- "partialCredit" #set the link function as 2PL IRT
  NodeSets(nd) <- c("pnodes","onodes","Observables")
  PnodeRules(nd) <- "Compensatory"
  PnodeQ(nd) <- TRUE
  PnodeLnAlphas(nd) <- 0
  PnodeBetas(nd) <-0
}

## Anchor nodes which tap only single skills./associate the items to thetas
## assign 2 items to theta0 and 3 items to theta1
NodeParents(obs$t0) <- list(thetas$theta0) #defines which child is to which parent
NodeParents(obs$t1) <- list(thetas$theta1)
NodeParents(obs$t2) <- list(thetas$theta0)
NodeParents(obs$t3) <- list(thetas$theta1)
NodeParents(obs$t4) <- list(thetas$theta1)

PnodePriorWeight(obs$t0) <- 25
PnodePriorWeight(obs$t1) <- 25
PnodePriorWeight(obs$t2) <- 25
PnodePriorWeight(obs$t3) <- 25
PnodePriorWeight(obs$t4) <- 25

PnodeRules(obs$comp) <- "Compensatory" #setting the rules
PnodeLnAlphas(obs$comp) <- c(theta0=-.25,theta1=-.25) #setting the slope paramater
PnodeBetas(obs$comp) <- .1 #Slightly hard # setting the difficulty parameter for each parent



## For conjunctive model make Theta 0 more important
PnodeRules(obs$conj) <- "Conjunctive" #setting the rules
PnodeLnAlphas(obs$conj) <- c(theta0=0,theta1=-.5) #setting the slope paramater
PnodeBetas(obs$conj) <- 1 #Slightly hard # setting the difficulty parameter for each parent

PnodeRules(obs$oconj) <- "OffsetConjunctive"
PnodeLnAlphas(obs$oconj) <- 0
PnodeBetas(obs$oconj) <- c(theta0=1,theta1=-1)

## Reverse parents of disjunctive models, to make sure we
## Get numbers in correct places.
NodeParents(obs$disj) <- rev(thetas)
PnodeRules(obs$disj) <- "Disjunctive"
PnodeLnAlphas(obs$disj) <- c(theta1=.5,theta0=-.5)
PnodeBetas(obs$disj) <- 0

NodeParents(obs$odis) <- rev(thetas)
PnodeRules(obs$odis) <- "OffsetDisjunctive"
PnodeLnAlphas(obs$odis) <- 0
PnodeBetas(obs$odis) <- c(theta1=.5,theta0=-.5)

BuildAllTables(binAll)

is.active(binAll) # check if the network is active
WriteNetworks(binAll,"binAll.dne")

# Function to build a blank Q-matrix from a Bayes net.
# binAll.Q <- Pnet2Qmat(binAll,obs,thetas,defaultBeta=0)

# write.csv(binAll.Q,"binall.Q.csv",na="",row.names=FALSE)

# binAll.Omega <- Pnet2Omega(binall,thetas)
# write.csv(binAll.Omega,"binall.Omega.csv",na="",row.names=FALSE)

##############################################################
## Generated Random Numbers in Netica Program
## and saved as "300twothetas10items.cas"
