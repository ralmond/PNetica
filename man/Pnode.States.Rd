\name{Pnode.States}
\Rdversion{1.1}
\docType{class}
\alias{Pnode.States}
\alias{PnodeNumStates,NeticaNode-method}
\alias{PnodeStates,NeticaNode-method}
\alias{PnodeStates<-,NeticaNode-method}
\alias{PnodeStateTitles,NeticaNode-method}
\alias{PnodeStateTitles<-,NeticaNode-method}
\alias{PnodeStateDescriptions,NeticaNode-method}
\alias{PnodeStateDescriptions<-,NeticaNode-method}
\alias{PnodeStateValues,NeticaNode-method}
\alias{PnodeStateValues<-,NeticaNode-method}
\alias{PnodeStateBounds,NeticaNode-method}
\alias{PnodeStateBounds<-,NeticaNode-method}

\title{States of the \code{"NeticaNode"} as a \code{"Pnode"}}
\description{

  The \code{PNetica} package supplies the needed methods so that the
  \code{RNetica::\linkS4class{NeticaNode}} object is an instance of the
  \code{Peanut::\linkS4class{Pnode}} object.  As a Pnode is nominally
  parameterized, the are given the special label \dQuote{pnode} to
  indicate that this note has parametric information.  This page
  documents the methods which access the states.

}
\section{Extends}{

  See \code{\linkS4class{NeticaNode}} for a description of the Netica class.

  With these methods, \code{\linkS4class{NeticaNode}} now extends
  \code{\linkS4class{Pnode}}. 


  All reference classes extend and inherit methods from
  \code{"\linkS4class{envRefClass}"}. 

}
\section{Methods}{

  All methods are implementations of generic functions in the
  \code{\link[Peanut:Peanut-package]{Peanut}} package.
  The following functions work with the states associated with the
  node.  Each of the values returned or set is a vector whose length
  should match the number of states of the node.

\describe{
    \item{\link[Peanut]{PnodeNumStates}}{\code{signature(node = NeticaNode)}:
      Returns the number of states of the node.}
    \item{\link[Peanut]{PnodeStates}}{\code{signature(node = NeticaNode)}:
      Gets the names of the states of the node.}
    \item{\link[Peanut]{PnodeStates<-}}{\code{signature(node = NeticaNode)}:
      Sets the number and names of the states of the node.  Note that
      node state names must follow the \link[RNetica]{IDname}
      conventions. }
    \item{\link[Peanut]{PnodeStateTitles}}{\code{signature(node = NeticaNode)}:
      Gets the titles of the states.}
    \item{\link[Peanut]{PnodeStateTitles<-}}{\code{signature(node = NeticaNode)}:
      Sets the titles of the states.}
    \item{\link[Peanut]{PnodeStateDescriptions}}{\code{signature(node =
	NeticaNode)}: Gets the description of the states.}
    \item{\link[Peanut]{PnodeStateDescriptions<-}}{\code{(signature(node =
	NeticaNode)}:  Sets the description of the states.}
    \item{\link[Peanut]{PnodeStateValues}}{\code{signature(node = NeticaNode)}:
      Gets the vector of real values associated with the states.  For
      continuous nodes, these are calculated from the bounds.}
    \item{\link[Peanut]{PnodeStateValues<-}}{\code{signature(node =
	NeticaNode)}:  Sets the vector of real values associated with
      the states; the node must be discrete.}
    \item{\link[Peanut]{PnodeStateBounds}}{\code{signature(node = NeticaNode)}:
      Gets the K by 2 matrix of upper and lower bounds for a continuous
      node. }
    \item{\link[Peanut]{PnodeStateBounds<-}}{\code{signature(node = 
	NeticaNode)}:  Sets the 
      K by 2 matrix of upper and lower bounds for a continuous
      node. The upper and lower bounds must match, see the documentation
      in the Peanut package for more information.}
    
  }
  
  Documentation for other methods of the \code{\linkS4class{Pnode}}
  generic functions for \code{\linkS4class{NeticaNode}} objects can be
  found in the documentation objects \link{Pnode.NeticaNode} and
  \link{Pnode.Properties}. 


}
\author{Russell Almond}
\seealso{

  Other methods of this class \code{\link{Pnode.NeticaNode}},
  \code{\link{Pnode.Properties}}. 

  Base class:  \code{\linkS4class{NeticaNode}}.

  Mixin class: \code{\linkS4class{Pnode}}.

  Generic functions from \code{Peanut} package:


  \code{\link[Peanut]{PnodeNumStates}},
  \code{\link[Peanut]{PnodeStates}},
  \code{\link[Peanut]{PnodeStateTitles}},
  \code{\link[Peanut]{PnodeStateDescriptions}},
  \code{\link[Peanut]{PnodeStateValues}},
  \code{\link[Peanut]{PnodeStateBounds}}.

  
}
\note{

  Netica overrides the \code{\link[RNetica]{NodeLevels}} to do different
  things whether the node is continuous or discrete.  The functions
  \code{\link[Peanut]{PnodeStateValues}} and
  \code{\link[Peanut]{PnodeStateBounds}} attempt to untangle these two
  different use cases.  In particular, \code{NodeLevels} for a continuous
  node assumes that the range of the node is chopped into a number of
  contiguous segments, and what is fed to the function is a list of cut
  points.  Thus, it will encouter problems if the lower bound of one
  state does not match the upper of the preious one.  


}
\examples{
sess <- NeticaSession()
startSession(sess)
curd <- setwd(system.file("testnets",package="PNetica"))

## Making states
anet <- CreateNetwork("Annette", session=sess)

## Discrete Nodes
nodel2 <- NewDiscreteNode(anet,"TwoLevelNode")
stopifnot(
  length(PnodeStates(nodel2))==2,
  PnodeStates(nodel2)==c("Yes","No")
)

PnodeStates(nodel2) <- c("True","False")
stopifnot(
  PnodeNumStates(nodel2) == 2L,
  PnodeStates(nodel2)==c("True","False")
)

nodel3 <- NewDiscreteNode(anet,"ThreeLevelNode",c("High","Med","Low"))
stopifnot(
  PnodeNumStates(nodel3) == 3L,
  PnodeStates(nodel3)==c("High","Med","Low"),
  PnodeStates(nodel3)[2]=="Med"
)

PnodeStates(nodel3)[2] <- "Median"
stopifnot(
  PnodeStates(nodel3)[2]=="Median"
)

PnodeStates(nodel3)["Median"] <- "Medium"
stopifnot(
  PnodeStates(nodel3)[2]=="Medium"
)


DeleteNetwork(anet)

## State Metadata (Titles and Descriptions)

cnet <- CreateNetwork("CreativeNet", session=sess)

orig <- NewDiscreteNode(cnet,"Originality", c("H","M","L"))
PnodeStateTitles(orig) <- c("High","Medium","Low")
PnodeStateDescriptions(orig)[1] <- "Produces solutions unlike those typically seen."

stopifnot(
  PnodeStateTitles(orig) == c("High","Medium","Low"),
  grep("solutions unlike", PnodeStateDescriptions(orig))==1,
  PnodeStateDescriptions(orig)[3]==""
  )

sol <- NewDiscreteNode(cnet,"Solution",
       c("Typical","Unusual","VeryUnusual"))
stopifnot(
  all(PnodeStateTitles(sol) == ""),
  all(PnodeStateDescriptions(sol) == "")
  )

PnodeStateTitles(sol)["VeryUnusual"] <- "Very Unusual"
PnodeStateDescriptions(sol) <- paste("Distance from typical solution",
                      c("<1", "1--2", ">2"))
stopifnot(
  PnodeStateTitles(sol)[3]=="Very Unusual",
  PnodeStateDescriptions(sol)[1] == "Distance from typical solution <1"
  )

DeleteNetwork(cnet)

## State Values
lnet <- CreateNetwork("LeveledNet", session=sess)

vnode <- NewDiscreteNode(lnet,"volt_switch",c("Off","Reverse","Forwards"))
stopifnot(
  length(PnodeStateValues(vnode))==3,
  names(PnodeStateValues(vnode)) == PnodeStates(vnode),
  all(is.na(PnodeStateValues(vnode)))
)

## Don't run this until the levels for vnode have been set,
## it will generate an error.
try(PnodeStateValues(vnode)[2] <- 0)

PnodeStateValues(vnode) <- 1:3
stopifnot(
  length(PnodeStateValues(vnode))==3,
  names(PnodeStateValues(vnode)) == PnodeStates(vnode),
  PnodeStateValues(vnode)[2]==2
)

PnodeStateValues(vnode)["Reverse"] <- -2

## Continuous nodes get the state values from the bounds.
theta0 <- NewContinuousNode(lnet,"theta0")
stopifnot(length(PnodeStateValues(theta0))==0L)
norm5 <- 
   matrix(c(qnorm(c(.001,.2,.4,.6,.8)),
            qnorm(c(.2,.4,.6,.8,.999))),5,2,
          dimnames=list(c("VH","High","Mid","Low","VL"),
                        c("LowerBound","UpperBound")))
PnodeStateBounds(theta0) <- norm5
PnodeStateValues(theta0)  ## Note these are medians not mean wrt normal!
PnodeStateBounds(theta0)[1,1] <- -Inf
PnodeStateValues(theta0)  ## Infinite value!


DeleteNetwork(lnet)

stopSession(sess)
setwd(curd)

}
\keyword{classes}
\keyword{graphs}
