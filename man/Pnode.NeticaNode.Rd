\name{Pnode.NeticaNode}
\Rdversion{1.1}
\docType{class}
\alias{Pnode.NeticaNode}
\alias{PnodeNet,NeticaNode-method}
\alias{PnodeName,NeticaNode-method}
\alias{PnodeName<-,NeticaNode-method}
\alias{PnodeTitle,NeticaNode-method}
\alias{PnodeTitle<-,NeticaNode-method}
\alias{PnodeDescription,NeticaNode-method}
\alias{PnodeDescription<-,NeticaNode-method}
\alias{PnodeLabels,NeticaNode-method}
\alias{PnodeLabels<-,NeticaNode-method}
\alias{PnodeProbs,NeticaNode-method}
\alias{PnodeProbs<-,NeticaNode-method}
\alias{PnodeNumParents,NeticaNode-method}
\alias{PnodeParentNames,NeticaNode-method}
\alias{PnodeParents,NeticaNode-method}
\alias{PnodeParents<-,NeticaNode-method}
\alias{isPnodeContinuous,NeticaNode-method}

\title{Class \code{"NeticaNode"} as a \code{"Pnode"}}
\description{

  The \code{PNetica} package supplies the needed methods so that the
  \code{RNetica::\linkS4class{NeticaNode}} object is an instance of the
  \code{Peanut::\linkS4class{Pnode}} object.  As a Pnode is nominally
  parameterized, the are given the special label \dQuote{pnode} to
  indicate that this note has parametric information.

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
  The following methods are related to the basic node structures and
  they should operate on all \code{\linkS4class{NeticaNode}} objects,
  whether they are \code{\linkS4class{Pnode}}s or not.

  \describe{
    \item{\link[Peanut]{PnodeNet}}{\code{signature(net = NeticaNode)}:
      Returns the \code{\linkS4class{NeticaBN}} (also
    \code{\linkS4class{Pnet}}) which contains the node.}
    \item{\link[Peanut]{PnodeName}}{\code{signature(net = NeticaNode)}:
      Gets the name of the node.}
    \item{\link[Peanut]{PnodeName<-}}{\code{signature(net = NeticaNode)}:
      Sets the name of the node.}
    \item{\link[Peanut]{PnodeTitle}}{\code{signature(net = NeticaNode)}:
      Gets the title of the node.}
    \item{\link[Peanut]{PnodeTitle<-}}{\code{signature(net = NeticaNode)}:
      Sets the title of the node.}
    \item{\link[Peanut]{PnodeDescription}}{\code{signature(net =
	NeticaNode)}: Gets the description of the node.}
    \item{\link[Peanut]{PnodeProbs}}{\code{signature(net = NeticaNode)}:
      Gets the conditional probability table for a node..}
    \item{\link[Peanut]{PnodeProbs<-}}{\code{signature(net = NeticaNode)}:
      Sets the conditional probability table for a node.}
    \item{\link[Peanut]{PnodeDescription<-}}{\code{(signature(net =
	NeticaNode)}:  Sets the description of the node.}
    \item{\link[Peanut]{PnodeLabels}}{\code{signature(net = NeticaNode)}:
      Gets the vector of names of the sets to which this node belongs.}
    \item{\link[Peanut]{PnodeLabels<-}}{\code{signature(net =
	NeticaNode)}:  Sets the vector of sets to which the node belongs.}
    \item{\link[Peanut]{isPnodeContinuous}}{\code{signature(net = NeticaNode)}:
      Returns true or false, depending on whether or not node is continuous.}
  }

  Documentation for other methods of the \code{\linkS4class{Pnode}}
  generic functions for \code{\linkS4class{NeticaNode}} objects can be
  found in the documentation objects \link{Pnode.Properties} and
  \link{Pnode.States}. 

}
\author{Russell Almond}
\seealso{

  Other methods of this class \code{\link{Pnode.States}},
  \code{\link{Pnode.Properties}}. 

  Base class:  \code{\linkS4class{NeticaNode}}.

  Mixin class: \code{\linkS4class{Pnode}}.

  Generic functions from \code{Peanut} package:

  \code{\link[Peanut]{Pnode}},
  \code{\link[Peanut]{PnodeNet}},
  \code{\link[Peanut]{PnodeName}},
  \code{\link[Peanut]{PnodeTitle}},
  \code{\link[Peanut]{PnodeDescription}},
  \code{\link[Peanut]{PnodeLabels}},
  \code{\link[Peanut]{PnodeNumParents}},
  \code{\link[Peanut]{PnodeParentNames}},
  \code{\link[Peanut]{PnodeParents}},
  \code{\link[Peanut]{PnodeProbs}},
  \code{\link[Peanut]{as.Pnode}},
  \code{\link[Peanut]{is.Pnode}},
  \code{\link[Peanut]{isPnodeContinuous}}.
  
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

nsnet <- CreateNetwork("NodeSetExample", session=sess)
Ability <- NewDiscreteNode(nsnet,"Ability",c("High","Med","Low"))
EssayScore <- NewDiscreteNode(nsnet,"EssayScore",paste("level",5:0,sep="_"))
Duration <- NewContinuousNode(nsnet,"Duration")

## Pnode, is.Pnode, as.Pnode
stopifnot(!is.Pnode(EssayScore),!is.Pnode(Duration))
EssayScore <- Pnode(EssayScore)
Duration <- as.Pnode(Duration)
stopifnot(is.Pnode(EssayScore),is.Pnode(Duration))

## PnodeNet

stopifnot(PnodeNet(Ability)==nsnet)

## PnodeName, PnodeTitle, PnodeDescription
PnodeTitle(Ability) <- "Student Ability"
PnodeDescription(Ability) <-
"Students who have more ability will have more success on the exam."
stopifnot(PnodeTitle(Ability) == "Student Ability",
PnodeDescription(Ability) ==
"Students who have more ability will have more success on the exam."
)


## PnodeLabels
stopifnot(
  length(PnodeLabels(Ability)) == 0L ## Nothing set yet
)
PnodeLabels(Ability) <- "ReportingVariable"
stopifnot(
  PnodeLabels(Ability) == "ReportingVariable"
)
PnodeLabels(EssayScore) <- c("Observable",PnodeLabels(EssayScore))
stopifnot(
  !is.na(match("Observable",PnodeLabels(EssayScore)))
)
## Make EssayScore a reporting variable, too
PnodeLabels(EssayScore) <- c("ReportingVariable",PnodeLabels(EssayScore))
stopifnot(
  setequal(PnodeLabels(EssayScore),c("Observable","ReportingVariable","pnodes"))
)

## Clear out the node set
PnodeLabels(Ability) <- character()
stopifnot(
  length(PnodeLabels(Ability)) == 0L
)

## PnodeNumParents, PnodeParents

stopifnot(PnodeNumParents(Ability)==0L, PnodeParents(Ability)==list())
PnodeParents(EssayScore) <- list(Ability)
stopifnot(PnodeNumParents(EssayScore)==1L,
          PnodeParents(EssayScore)[[1]]==Ability,
          PnodeParentNames(EssayScore)=="Ability")

DeleteNetwork(nsnet)

## Node Probs
abc <- CreateNetwork("ABC", session=sess)
A <- NewDiscreteNode(abc,"A",c("A1","A2","A3","A4"))
B <- NewDiscreteNode(abc,"B",c("B1","B2","B3"))
C <- NewDiscreteNode(abc,"C",c("C1","C2"))

PnodeParents(A) <- list()
PnodeParents(B) <- list(A)
PnodeParents(C) <- list(A,B)

PnodeProbs(A)<-c(.1,.2,.3,.4)
PnodeProbs(B) <- normalize(matrix(1:12,4,3))
PnodeProbs(C) <- normalize(array(1:24,c(A=4,B=3,C=2)))

Aprobs <- PnodeProbs(A)
Bprobs <- PnodeProbs(B)
Cprobs <- PnodeProbs(C)
stopifnot(
  CPTtools::is.CPA(Aprobs),
  CPTtools::is.CPA(Bprobs),
  CPTtools::is.CPA(Cprobs)
)

DeleteNetwork(abc)



stopSession(sess)

}
\keyword{classes}
\keyword{graphs}
