\name{Pnode.Properties}
\Rdversion{1.1}
\docType{class}
\alias{Pnode.Properties}
\alias{as.Pnode,NeticaNode-method}
\alias{is.Pnode,NeticaNode-method}
\alias{Pnode,NeticaNode-method}
\alias{PnodeLnAlphas,NeticaNode-method}
\alias{PnodeLnAlphas<-,NeticaNode-method}
\alias{PnodeBetas,NeticaNode-method}
\alias{PnodeBetas<-,NeticaNode-method}
\alias{PnodeQ,NeticaNode-method}
\alias{PnodeQ<-,NeticaNode-method}
\alias{PnodeRules,NeticaNode-method}
\alias{PnodeRules<-,NeticaNode-method}
\alias{PnodeLink,NeticaNode-method}
\alias{PnodeLink<-,NeticaNode-method}
\alias{PnodeLinkScale,NeticaNode-method}
\alias{PnodeLinkScale<-,NeticaNode-method}
\alias{PnodePostWeight,NeticaNode-method}
\alias{PnodePriorWeight,NeticaNode-method}
\alias{PnodePriorWeight<-,NeticaNode-method}

\title{Properties of class \code{"NeticaNode"} as a \code{"Pnode"}}
\description{

  The \code{PNetica} package supplies the needed methods so that the
  \code{RNetica::\linkS4class{NeticaNode}} object is an instance of the
  \code{Peanut::\linkS4class{Pnode}} object.  As a Pnode is nominally
  parameterized, the are given the special label \dQuote{pnode} to
  indicate that this note has parametric information.  This document
  describes the extra properties of \code{\linkS4class{Pnode}}s that are
  added by \code{PNetica}.

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
  These methods are related to the parameteric information which makes a
  node a \code{\linkS4class{Pnode}}.  To inidcate that a node has this
  extra information, it should have the \dQuote{"pnode"} label.  The
  functions \code{\link[Peanut]{Pnode}} and
  \code{\link[Peanut]{as.Pnode}} will do this.

  \describe{
    \item{\link[Peanut]{Pnode}}{\code{signature(node =
	"NeticaNode", lnAlphas, betas, rules = "Compensatory", link =
	"partialCredit", Q = TRUE, linkScale = NULL, priorWeight = NULL)}:
      This function forces a  \code{\linkS4class{NeticaNode}} into a
      \code{\link[Peanut]{Pnode}} by initializing the Pnode-specific fields. }
    \item{\link[Peanut]{PnodeLnAlphas}}{\code{signature(node = NeticaNode)}:
      Returns the log of discrimination parameters associated with the
    node.}
    \item{\link[Peanut]{PnodeLnAlphas<-}}{\code{signature(node = NeticaNode)}:
      Sets the log of discrimination parameters associated with the
    node.}
    \item{\link[Peanut]{PnodeBetas}}{\code{signature(node = NeticaNode)}:
      Returns the difficulty parameters associated with the
    node.}
    \item{\link[Peanut]{PnodeBetas<-}}{\code{signature(node = NeticaNode)}:
      Sets the difficulty parameters associated with the
    node.}
    \item{\link[Peanut]{PnodeQ}}{\code{signature(node = NeticaNode)}:
      Returns the local Q matrix associated with the
    node.}
    \item{\link[Peanut]{PnodeQ<-}}{\code{signature(node = NeticaNode)}:
      Sets the local Q matrix associated with the
    node.}
    \item{\link[Peanut]{PnodeRules}}{\code{signature(node = NeticaNode)}:
      Returns the names of the combination rules associated with the
    node.}
    \item{\link[Peanut]{PnodeRules<-}}{\code{signature(node = NeticaNode)}:
      Sets the names of the combination rules associated with the
    node.}
    \item{\link[Peanut]{PnodeLink}}{\code{signature(node = NeticaNode)}:
      Returns the link function associated with the
    node.}
    \item{\link[Peanut]{PnodeLink<-}}{\code{signature(node = NeticaNode)}:
      Sets the link function associated with the
    node.}
    \item{\link[Peanut]{PnodeLinkScale}}{\code{signature(node = NeticaNode)}:
      Returns the link function scale parameter associated with the
    node.}
    \item{\link[Peanut]{PnodeLinkScale<-}}{\code{signature(node = NeticaNode)}:
      Sets the link function scale parameter associated with the
    node.}
    \item{\link[Peanut]{PnodePriorWeight}}{\code{signature(node = NeticaNode)}:
      Returns the weight or weights assigned to prior information
      associated with the  node.}
    \item{\link[Peanut]{PnodePriorWeight<-}}{\code{signature(node = NeticaNode)}:
      Sets the weight or weights assigned to prior information
      associated with the  node.}
    \item{\link[Peanut]{PnodePostWeight}}{\code{signature(node = NeticaNode)}:
      Returns the combined prior and data weights 
      associated with the  node.}
    \item{\link[Peanut]{as.Pnode}}{\code{signature(x = NeticaNode)}:
      Forces \code{x} to be a \code{\linkS4class{Pnode}}; in particular,
      it adds the lable "pnode".}
    \item{\link[Peanut]{is.Pnode}}{\code{signature(x = NeticaNode)}:
      Returns true if the node has the special label "pnode".}
  }

  Documentation for other methods of the \code{\linkS4class{Pnode}}
  generic functions for \code{\linkS4class{NeticaNode}} objects can be
  found in the documentation objects \link{Pnode.NeticaNode} and
  \link{Pnode.States}. 

}
\author{Russell Almond}
\seealso{

  Other methods of this class \code{\link{Pnode.NeticaNode}},
  \code{\link{Pnode.Properties}}. 

  Base class:  \code{\linkS4class{NeticaNode}}.

  Mixin class: \code{\linkS4class{Pnode}}.

  Generic functions from \code{Peanut} package:

  \code{\link[Peanut]{PnodeLnAlphas}},
  \code{\link[Peanut]{PnodeBetas}},
  \code{\link[Peanut]{PnodeQ}},
  \code{\link[Peanut]{PnodeRules}},
  \code{\link[Peanut]{PnodeLink}},
  \code{\link[Peanut]{PnodeLinkScale}},
  \code{\link[Peanut]{PnodePostWeight}},
  \code{\link[Peanut]{PnodePriorWeight}}.


  
}
\note{

  The \dQuote{Pnode properies},
  \code{lnAlphas}, \code{betas}, \code{Q}, \code{rules}, \code{link},
  \code{linkScale}, and \code{priorWeight} are stored in user fields
  (\code{\link{NodeUserObj}}) of the Netica node.  A
  \code{\linkS4class{NeticaNode}} object which has those fields behaves
  as a \code{\linkS4class{Pnode}} and is suitable for the use with
  \code{\link[Peanut:Peanut-package]{Peanut}}.  The function
  \code{Pnode} will add default values for these fields if they are not
  set.  

  To mark a node as a \code{Pnode}, it is added to the
  \link[RNetica:NodeSets]{node set} \dQuote{pnode}.  The \code{is.Pnode}
  function checks for this method.


}
\examples{
sess <- NeticaSession()
startSession(sess)
curd <- setwd(system.file("testnets",package="PNetica"))

tNet <- CreateNetwork("TestNet",sess)

## Alphas
theta1 <- NewDiscreteNode(tNet,"theta1",
                         c("VH","High","Mid","Low","VL"))
PnodeStateValues(theta1) <- effectiveThetas(PnodeNumStates(theta1))
PnodeProbs(theta1) <- rep(1/PnodeNumStates(theta1),PnodeNumStates(theta1))
theta2 <- NewDiscreteNode(tNet,"theta2",
                         c("VH","High","Mid","Low","VL"))
PnodeStateValues(theta2) <- effectiveThetas(PnodeNumStates(theta2))
PnodeProbs(theta2) <- rep(1/PnodeNumStates(theta1),PnodeNumStates(theta2))

partial3 <- NewDiscreteNode(tNet,"partial3",
                            c("FullCredit","PartialCredit","NoCredit"))
PnodeParents(partial3) <- list(theta1,theta2)

## Usual way to set rules is in constructor
partial3 <- Pnode(partial3,rules="Compensatory", link="partialCredit")
PnodePriorWeight(partial3) <- 10
BuildTable(partial3)

## slopes of 1 for both transitions
PnodeLnAlphas(partial3) <- c(0,0)
BuildTable(partial3)

## log slope 0 = slope 1
stopifnot(
   all(abs(PnodeAlphas(partial3) -1) <.0001)
)

## Make Skill 1 more important than Skill 2
PnodeLnAlphas(partial3) <- c(.25,-.25)
BuildTable(partial3)

## increasing intercepts for both transitions
PnodeLink(partial3) <- "gradedResponse"
PnodeBetas(partial3) <- list(FullCredit=1,PartialCredit=0)
BuildTable(partial3)
stopifnot(
   all(abs(do.call("c",PnodeBetas(partial3)) -c(1,0) ) <.0001)
)


## increasing intercepts for both transitions
PnodeLink(partial3) <- "partialCredit"
## Full Credit is still rarer than partial credit under the partial
## credit model
PnodeBetas(partial3) <- list(FullCredit=0,PartialCredit=0)
BuildTable(partial3)
stopifnot(
   all(abs(do.call("c",PnodeBetas(partial3)) -c(0,0) ) <.0001)
)


## Make Skill 1 more important for the transition to ParitalCredit
## And Skill 2 more important for the transition to FullCredit
PnodeLnAlphas(partial3) <- list(FullCredit=c(-.25,.25),
                                PartialCredit=c(.25,-.25))
BuildTable(partial3)

## Set up so that first skill only needed for first transition, second
## skill for second transition; Adjust alphas to match
PnodeQ(partial3) <- matrix(c(TRUE,TRUE,
                             TRUE,FALSE), 2,2, byrow=TRUE)
PnodeLnAlphas(partial3) <- list(FullCredit=c(-.25,.25),
                                PartialCredit=0)
BuildTable(partial3)

## Using OffsetConjunctive rule requires single slope
PnodeRules(partial3) <- "OffsetConjunctive"
## Single slope parameter for each transition
PnodeLnAlphas(partial3) <- 0
PnodeQ(partial3) <- TRUE
PnodeBetas(partial3) <- c(0,1)
BuildTable(partial3)

## Make Skill 1 more important for the transition to ParitalCredit
## And Skill 2 more important for the transition to FullCredit
PnodeLnAlphas(partial3) <- 0
PnodeBetas(partial3) <- list(FullCredit=c(-.25,.25),
                                PartialCredit=c(.25,-.25))
BuildTable(partial3)


## Separate slope parameter for each transition;  
## Note this will only different from the previous transition when
## mapDPC is called.  In the former case, it will learn a single slope
## parameter, in the latter, it will learn a different slope for each
## transition. 
PnodeLnAlphas(partial3) <- list(0,0)
BuildTable(partial3)

## Set up so that first skill only needed for first transition, second
## skill for second transition; Adjust betas to match
PnodeQ(partial3) <- matrix(c(TRUE,TRUE,
                             TRUE,FALSE), 2,2, byrow=TRUE)
PnodeBetas(partial3) <- list(FullCredit=c(-.25,.25),
                                PartialCredit=0)
BuildTable(partial3)


## Can also do this with special parameter values
PnodeQ(partial3) <- TRUE
PnodeBetas(partial3) <- list(FullCredit=c(-.25,.25),
                                PartialCredit=c(0,Inf))
BuildTable(partial3)

## The normal link function is the only one which takes a scale parameter
PnodeLink(partial3) <- "normalLink"
PnodeLinkScale(partial3) <- 1.0
PnodeLnAlphas(partial3) <- 0
PnodeBetas(partial3) <- c(0,1)
BuildTable(partial3)
stopifnot(
  all(abs(PnodeLinkScale(partial3)-1)<.0001)
)

DeleteNetwork(tNet)

stopSession(sess)
setwd(curd)

}
\keyword{classes}
\keyword{graphs}
