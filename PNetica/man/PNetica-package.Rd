\name{PNetica-package}
\alias{PNetica-package}
\alias{PNetica}
\docType{package}
\title{
\packageTitle{PNetica}
}
\description{
\packageDescription{PNetica}
}
\details{

The DESCRIPTION file:
\packageDESCRIPTION{PNetica}

The \code{\link[Peanut]{Peanut}} package provides a set of generic
functions for manipulation parameterized networks, in particular, for
the abstract \code{\link[Peanut]{Pnet}} and \code{\link[Peanut]{Pnode}}
classes.  This package provides concrete implemenations of those classes
using the built in classes of \code{\link[RNetica]{RNetica}}.  In
particular, \code{\link{Pnet.NeticaBN}} extends
\code{\link[RNetica]{NeticaBN}} and \code{\link{Pnode.NeticaNode}} extends
\code{\link[RNetica]{NeticaNode}}.

The properties of the \code{\link[Peanut]{Pnet}} and
\code{\link[Peanut]{Pnode}} objects are stored as serialized Netica user
fields (see \code{\link[RNetica]{NetworkUserObj}} and
\code{\link[RNetica]{NodeUserObj}}).  

The \code{as.Pnet} (\code{as.Pnode}) method for
a \code{\link[RNetica]{NeticaBN}} (\code{\link[RNetica]{NeticaNode}})
merely adds \dQuote{Pnet} (\dQuote{Pnode}) to \code{class(net)}
(\code{class(node)}).  All of the methods in the \code{PNetica} are
defined for either the \code{\link[RNetica]{NeticaBN}} or
\code{\link[RNetica]{NeticaNode}} object, so strictly speaking, adding
the \dQuote{Pnet} or \dQuote{Pnode} class is not necessary, but it is
recommended in case this is used in the future.

}
\section{PNetica Specific Implemenation Details}{

  Here are some Netica specific details which may not be apparent from
  the description of the generic functions in the
  \code{\link[Peanut]{Peanut}} package.

  \enumerate{
    \item{The \code{cases} argument to \code{\link{calcPnetLLike.NeticaBN}},
      \code{\link{calcExpTables.NeticaBN}} and
      \code{\link[Peanut]{GEMfit}} all 
      expect the pathname of a Netica case file (see
      \code{\link[RNetica]{write.CaseFile}}). }
    \item{The methods \code{\link{calcPnetLLike.NeticaBN}},
      \code{\link{calcExpTables.NeticaBN}}, and therefore 
      \code{\link[Peanut]{GEMfit}} when called with a
      \code{\link{Pnet.NeticaBN}} as the first argument, expect that
      there exists a node set (see
      \code{\link[RNetica]{NetworkNodesInSet}}) called \dQuote{onodes}
      corresponding to 
      the observable variables in the case file \code{cases}.}
    \item{The function \code{\link[RNetica]{CompileNetwork}} needs to be
      called before calls to \code{\link{calcPnetLLike.NeticaBN}},
      \code{\link{calcExpTables.NeticaBN}} and
      \code{\link[Peanut]{GEMfit}}.}
    \item{The method \code{\link{PnetPnodes.NeticaBN}} stores its value
      in a nodeset called \dQuote{pnodes}.  It is recommended that the
      accessor function be used for modifying this field.}
    \item{The \code{\link{PnetPriorWeight.NeticaBN}} field of the
      \code{\link{Pnet.NeticaBN}} object and all of the fields of the
      \code{\link{Pnode.NeticaNode}} are stored in serialized user fields
      with somewhat obvious names (see
      \code{\link[RNetica]{NetworkUserObj}} and 
      \code{\link[RNetica]{NodeUserObj}}).  These fields should not be
      used for other purposes.}
    }
}
\section{Creating and Restoring Pnet.NeticaBN objects}{

As both the nodesets and and user fields are serialized when Netica
serializes a network (\code{\link[RNetica]{WriteNetworks}}) the
fields of the \code{\link{Pnet.NeticaBN}} and
\code{\link{Pnode.NeticaNode}} objects should be properly saved and
restored.  The only thing which will not be restored is the code
\dQuote{Pnet} or \dQuote{Pnode} class marker.  These can be restored by
calling \code{\link[Peanut]{as.Pnet}} on the restored network and
\code{\link[Peanut]{as.Pnode}} on each of the restored \code{Pnode}s
(see Examples).  

The first time the network and nodes are created, it is recommended that
\code{\link[Peanut]{Pnet.default}} and \code{\link{Pnode.NeticaNode}}
(or simply the generic functions \code{\link[Peanut]{Pnet}} and
\code{\link[Peanut]{Pnode}}.  Note that calling
\code{\link{Pnode.NeticaNode}} will calculate defaults for the
\code{\link[Peanut]{PnodeLnAlphas}} and \code{\link[Peanut]{PnodeBetas}}
based on the current value of \code{\link[RNetica]{NodeParents}(node)},
so this should be set before calling this function. (See examples).

}
\section{Index}{
  \packageIndices{PNetica}
}
\author{
\packageAuthor{PNetica}

Maintainer: \packageMaintainer{PNetica}
}
\references{

  Almond, R. G. (2015) An IRT-based Parameterization for Conditional
  Probability Tables.  Paper presented at the 2015 Bayesian Application 
  Workshop at the Uncertainty in Artificial Intelligence Conference.

}
\keyword{ package }
\keyword{ graphs }
\seealso{

  \code{PNetica} depends ont the following other packages.
  \describe{
    \item{\code{\link[RNetica]{RNetica}}}{A binding
      of the Netica C API into R.}
    \item{\code{\link[Peanut:Peanut-package]{Peanut}}}{An
      the generic functions for which this package provides
      implementations.}
    \item{\code{\link[CPTtools:CPTtools-package]{CPTtools}}}{A
      collection of implemenation independent Bayes net utilities.}
    }

}
\section{Legal Stuff}{

  Netica and Norsys are registered trademarks of Norsys, LLC
  (\url{http://www.norsys.com/}), used by permission. 

  Extensive use of \code{PNetica} will require a Netica API license from
  Norsys.  This is basically a requirement of the
  \code{\link[RNetica]{RNetica}} package, and details are described more
  fully there.  Without a license, \code{RNetica} and \code{PNetica}
  will work in a student/demonstration mode which limits the size of the
  network. 

  Although Norsys is generally supportive of the RNetica project, it
  does not officially support RNetica, and all questions should be sent
  to the package maintainers.

}
\examples{

## Building CPTs
tNet <- CreateNetwork("TestNet")


theta1 <- NewDiscreteNode(tNet,"theta1",
                         c("VH","High","Mid","Low","VL"))
NodeLevels(theta1) <- effectiveThetas(NodeNumStates(theta1))
NodeProbs(theta1) <- rep(1/NodeNumStates(theta1),NodeNumStates(theta1))
theta2 <- NewDiscreteNode(tNet,"theta2",
                         c("VH","High","Mid","Low","VL"))
NodeLevels(theta2) <- effectiveThetas(NodeNumStates(theta2))
NodeProbs(theta2) <- rep(1/NodeNumStates(theta2),NodeNumStates(theta2))

partial3 <- NewDiscreteNode(tNet,"partial3",
                            c("FullCredit","PartialCredit","NoCredit"))
NodeParents(partial3) <- list(theta1,theta2)

partial3 <- Pnode(partial3,Q=TRUE, link="partialCredit")
PnodePriorWeight(partial3) <- 10
BuildTable(partial3)

## Set up so that first skill only needed for first transition, second
## skill for second transition; adjust alphas to match
PnodeQ(partial3) <- matrix(c(TRUE,TRUE,
                             TRUE,FALSE), 2,2, byrow=TRUE)
PnodeLnAlphas(partial3) <- list(FullCredit=c(-.25,.25),
                                PartialCredit=0)
BuildTable(partial3)
partial4 <- NewDiscreteNode(tNet,"partial4",
                            c("Score4","Score3","Score2","Score1"))
NodeParents(partial4) <- list(theta1,theta2)
partial4 <- Pnode(partial4, link="partialCredit")
PnodePriorWeight(partial4) <- 10

## Skill 1 used for first transition, Skill 2 used for second
## transition, both skills used for the 3rd.

PnodeQ(partial4) <- matrix(c(TRUE,TRUE,
                             FALSE,TRUE,
                             TRUE,FALSE), 3,2, byrow=TRUE)
PnodeLnAlphas(partial4) <- list(Score4=c(.25,.25),
                                Score3=0,
                                Score2=-.25)
BuildTable(partial4)

## Fitting Model to data

irt10.base <- ReadNetworks(paste(library(help="PNetica")$path,
                           "testnets","IRT10.2PL.base.dne",
                           sep=.Platform$file.sep))
irt10.base <- as.Pnet(irt10.base)  ## Flag as Pnet, fields already set.
irt10.theta <- NetworkFindNode(irt10.base,"theta")
irt10.items <- PnetPnodes(irt10.base)
## Flag items as Pnodes
for (i in 1:length(irt10.items)) {
  irt10.items[[i]] <- as.Pnode(irt10.items[[i]])
  
}


casepath <- paste(library(help="PNetica")$path,
                           "testdat","IRT10.2PL.200.items.cas",
                           sep=.Platform$file.sep)
## Record which nodes in the casefile we should pay attention to
NetworkNodesInSet(irt10.base,"onodes") <-
   NetworkNodesInSet(irt10.base,"observables")


BuildAllTables(irt10.base)
CompileNetwork(irt10.base) ## Netica requirement

item1 <- irt10.items[[1]]
priB <- PnodeBetas(item1)
priA <- PnodeAlphas(item1)
priCPT <- NodeProbs(item1)

gemout <- GEMfit(irt10.base,casepath)


DeleteNetwork(irt10.base)
DeleteNetwork(tNet)


}
