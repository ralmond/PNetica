\name{maxCPTParam.NeticaNode}
\alias{maxCPTParam,NeticaNode-method}
\title{Find optimal parameters of a Pnode.NeticaNode to match expected tables}
\description{

  These function assumes that an expected count contingency table can be
  built from the network; i.e., that \code{\link[RNetica]{LearnCPTs}}
  has been recently called.  They then try to find the set of parameters
  maximizes the probability of the expected contingency table with
  repeated calls to \code{\link[CPTtools]{mapDPC}}.  This describes the
  method for \code{\link[Peanut]{maxCPTParam}} when the
  \code{\link[Peanut]{Pnode}} is a \code{\link[RNetica]{NeticaNode}}.

}
\usage{
\S4method{maxCPTParam}{NeticaNode}(node, Mstepit = 5, tol = sqrt(.Machine$double.eps))
}
\arguments{
  \item{node}{A \code{\link{Pnode}} object giving the parameterized node.}
  \item{Mstepit}{A numeric scalar giving the number of maximization
    steps to take.  Note that the maximization does not need to be run
    to convergence.} 
  \item{tol}{A numeric scalar giving the stopping tolerance for the
    maximizer.}
}
\details{

  This method is called on on a \code{\link{Pnode.NeticaNode}} object
  during the M-step of the EM algorithm (see
  \code{\link[Peanut]{GEMfit}} and
  \code{\link[Peanut]{maxAllTableParams}} for details).  Its purpose is
  to extract the expected contingency table from Netica and pass it
  along to \code{\link[CPTtools]{mapDPC}}.

  When doing EM learning with Netica, the resulting conditional
  probability table (CPT) is the mean of the Dirichlet posterior.  Going
  from the mean to the parameter requires multiplying the CPT by row
  counts for the number of virtual observations.  In Netica, these are
  call \code{\link[RNetica]{NodeExperience}}.  Thus, the expected counts
  are calculated with this expression: 
  \code{sweep(node[[]], 1, NodeExperience(node), "*")}.

  What remains is to take the table of expected counts and feed it into
  \code{\link[CPTtools]{mapDPC}} and then take the output of that
  routine and update the parameters.

  The parameters \code{Mstepit} and \code{tol} are passed to
  \code{\link[CPTtools]{mapDPC}} to control the gradient decent
  algorithm used for maximization.  Note that for a generalized EM
  algorithm, the M-step does not need to be run to convergence, a couple
  of iterations are sufficient.  The value of \code{Mstepit} may
  influence the speed of convergence, so the optimal value may vary by
  application.  The tolerance is largely
  irrelevant (if \code{Mstepit} is small) as the outer EM algorithm does
  the tolerance test. 

  
}
\value{

  The expression \code{maxCPTParam(node)} returns \code{node} invisibly.
  As a side effect the \code{\link{PnodeLnAlphas}} and
  \code{\link{PnodeBetas}} fields of \code{node} (or
  all nodes in \code{\link{PnetPnodes}(net)}) are updated to better fit
  the expected tables.

}
\references{

  Almond, R. G. (2015) An IRT-based Parameterization for Conditional
  Probability Tables.  Paper presented at the 2015 Bayesian Application 
  Workshop at the Uncertainty in Artificial Intelligence Conference.

}
\author{Russell Almond}
\seealso{

  \code{\link[Peanut]{Pnode}}, \code{\link{Pnode.NeticaNode}},
  \code{\link[Peanut]{GEMfit}}, \code{\link[Peanut]{maxAllTableParams}}
  \code{\link[CPTtools]{mapDPC}}

}
\examples{

## This method is mostly a wrapper for CPTtools::mapDPC
getMethod(maxCPTParam,"NeticaNode")


sess <- NeticaSession()
startSession(sess)

irt10.base <- ReadNetworks(system.file("testnets","IRT10.2PL.base.dne",
                           package="PNetica"),
                           session=sess)
irt10.base <- as.Pnet(irt10.base)  ## Flag as Pnet, fields already set.
irt10.theta <- NetworkFindNode(irt10.base,"theta")
irt10.items <- PnetPnodes(irt10.base)
## Flag items as Pnodes
for (i in 1:length(irt10.items)) {
  irt10.items[[i]] <- as.Pnode(irt10.items[[i]])
  ## Add node to list of observed nodes
  PnodeLabels(irt10.items[[1]]) <-
     union(PnodeLabels(irt10.items[[1]]),"onodes")
}

casepath <- system.file("testdat","IRT10.2PL.200.items.cas",
                        package="PNetica")



BuildAllTables(irt10.base)
PnetCompile(irt10.base) ## Netica requirement

item1 <- irt10.items[[1]]
priB <- PnodeBetas(item1)
priA <- PnodeAlphas(item1)
priCPT <- PnodeProbs(item1)

gemout <- GEMfit(irt10.base,casepath,trace=TRUE)

calcExpTables(irt10.base,casepath)

maxAllTableParams(irt10.base)

postB <- PnodeBetas(item1)
postA <- PnodeAlphas(item1)
BuildTable(item1)
postCPT <- PnodeProbs(item1)

## Posterior should be different
stopifnot(
  postB != priB, postA != priA
)


DeleteNetwork(irt10.base)
stopSession(sess)


}
\keyword{ manip }

