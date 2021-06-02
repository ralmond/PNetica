\name{PnodeEvidence.NeticaNode}
\alias{PnodeEvidence.NeticaNode}
\alias{PnodeEvidence,NeticaNode-method}
\alias{PnodeEvidence<-,NeticaNode,ANY-method}
\alias{PnodeEvidence<-,NeticaNode,NULL-method}
\alias{PnodeEvidence<-,NeticaNode,character-method}
\alias{PnodeEvidence<-,NeticaNode,difftime-method}
\alias{PnodeEvidence<-,NeticaNode,factor-method}
\alias{PnodeEvidence<-,NeticaNode,logical-method}
\alias{PnodeEvidence<-,NeticaNode,numeric-method}
\title{Gets or sets the value of a Pnode.}
\description{

  Adding evidence to a Bayesian network is done by setting the value of
  the node to one of its states.  The generic function
  \code{Peanut::\link[Peanut]{PnodeEvidence}} (and the method for a
  \code{\linkS4class{NeticaNode}}) simply returns the to which it is set, or
  \code{NA} if the node is not set.   There are a number of different
  ways of setting the state depending on the type of the value argument
  (see Details).

}
\usage{
\S4method{PnodeEvidence}{NeticaNode}(node)
\S4method{PnodeEvidence}{NeticaNode,ANY}(node) <- value
}
\arguments{
  \item{node}{A \code{\linkS4class{NeticaNode}} object whose value is to
    be set.}
  \item{value}{A value representing the new type of the argument.  See
    details. }
}
\details{

  The generic function \code{\link[Peanut]{PnodeEvidence}} is defined in
  the \code{Peanut} package.  It returns either the name of a state
  (discrete node), a numeric value (continuous node) or \code{NA} if the
  node has not been set.

  There are different methods for different classes for the \code{value}
  argument (the RHS of the assignment operator).

  \describe{

    \item{ANY}{If no other method is appropriate, does nothing and
      issues a warning.}

    \item{NULL}{The value of the node is retracted
      (\code{\link[RNetica]{RetractNodeFinding}}). }

    \item{character}{If the \code{value} is the name of a state, then
      the node will be set to that state
      (\code{\link[RNetica]{NodeFinding}}).  Otherwise, nothing will be 
      done and a warning will be issued.}

    \item{factor}{The character value of the \code{value} is uses (see
      character method).}

    \item{logical}{This method assumes that the node has exactly two
      states, and that those states have values
      (\code{\link[Peanut]{PnodeStateValues}},
      \code{\link[RNetica]{NodeLevels}}) 0 and 1.  These levels are used
      to determine the mapping of TRUE and FALSE to states.  If node
      state values are not set, then the character method is called
      using \dQuote{TRUE} or \dQuote{FALSE} as the value.
    }

    \item{numeric}{If the \code{value} is of length 1, then the value of
      the node is set (\code{\link[RNetica]{NodeValue}}) to the argument.
      If the \code{value} is a vector of the same length as the number of
      states of the node, then it is regarded as virtual evidence, and the
      likelihood is set (\code{\link[RNetica]{NodeLikelihood}}). }

    \item{difftime}{Difftime \code{value}s are converted to real numbers
      in seconds, then the node value is set (see numeric method).}
  }


}
\value{

  \code{PnodeEvidence}:
  For all node types, if the node is not set, \code{PnodeEvidence}
  returns \code{NA}.

  If the node is continuous, its currently set value is returned as a
  numeric scalar (\code{NA} if not set).

  If the node is discrete, usually a character value giving the current
  state (or \code{NA}) is returned.  However, if the node was assigned a
  likelihood instead of exact evidence, the likelihood vector is
  returned. 
    
  \code{PnodeEvidence<-} returns the node argument invisibly. 

}
\author{Russell Almond}
\note{

  For continuous nodes, \code{PnodeEvidence} is equivalent to
  \code{\link[RNetica]{NodeValue}}.  For discrete nodes, it maps to
  either \code{\link[RNetica]{NodeFinding}} or
  \code{\link[RNetica]{NodeLikelihood}}

}
\seealso{
  

  The function \code{\link{PnetCompile}} usually needs to be run before
  this function has meaning.

  The functions \code{\link{PnodeStates}} and
  \code{\link{PnodeStateBounds}} define the legal values for the value
  argument. 
}
\examples{
sess <- NeticaSession()
startSession(sess)

irt10.base <- ReadNetworks(paste(library(help="PNetica")$path,
                           "testnets","IRT10.2PL.base.dne",
                           sep=.Platform$file.sep),session=sess)
irt10.base <- as.Pnet(irt10.base)  ## Flag as Pnet, fields already set.
irt10.theta <- PnetFindNode(irt10.base,"theta")
irt10.items <- PnetPnodes(irt10.base)
## Flag items as Pnodes
for (i in 1:length(irt10.items)) {
  irt10.items[[i]] <- as.Pnode(irt10.items[[i]])
  
}

BuildAllTables(irt10.base)
PnetCompile(irt10.base) ## Netica requirement

stopifnot (is.na(PnodeEvidence(irt10.items[[1]])))

PnodeEvidence(irt10.items[[1]]) <- "Correct"
stopifnot(PnodeEvidence(irt10.items[[1]])=="Correct")

PnodeEvidence(irt10.items[[1]]) <- NULL
stopifnot (is.na(PnodeEvidence(irt10.items[[1]])))

PnodeEvidence(irt10.items[[1]]) <- c(Correct=.6,Incorrect=.3)
stopifnot(all.equal(PnodeEvidence(irt10.items[[1]]),
                    c(Correct=.6,Incorrect=.3),
                    tol=3*sqrt(.Machine$double.eps) ))

foo <- NewContinuousNode(irt10.base,"foo")

stopifnot(is.na(PnodeEvidence(foo)))

PnodeEvidence(foo) <- 1
stopifnot(PnodeEvidence(foo)==1)

DeleteNetwork(irt10.base)
stopSession(sess)
}
\keyword{ graphs }
