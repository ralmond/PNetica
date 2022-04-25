\name{calcExpTables.NeticaBN}
\alias{calcExpTables,NeticaBN-method}
\title{Calculate expected tables for a Pnet.NeticaBN}
\description{

  The performs the E-step of the GEM algorithm by running the Netica 
  EM algorithm (see \code{\link[RNetica]{LearnCPTs}}) using the data in
  \code{cases}.  After this is run, the conditional probability table
  for each \code{\link{Pnode.NeticaNode}} should be the mean of the
  Dirichlet distribution and the scale parameter should be the value of
  \code{\link[RNetica]{NodeExperience}(node)}.  


}
\usage{
\S4method{calcExpTables}{NeticaBN}(net, cases, Estepit = 1,
                         tol = sqrt(.Machine$double.eps))
}
\arguments{
  \item{net}{A \code{\link{Pnet.NeticaBN}} object representing a
    parameterized network.} 
  \item{cases}{A character scalar giving the file name of a Netica case file
    (see \code{\link[RNetica]{write.CaseFile}}).}
  \item{Estepit}{An integer scalar describing the number of steps the
    Netica should take in the internal EM algorithm.}
  \item{tol}{A numeric scalar giving the stopping tolerance for the
    internal Netica EM algorithm.}

}
\details{

  The key to this method is realizing that the EM algorithm built into
  the Netica (see \code{\link[RNetica]{LearnCPTs}}) can perform the
  E-step of the outer \code{\link[Peanut]{GEMfit}} generalized EM
  algorithm.  It does this in every iteration of the algorithm, so one
  can stop after the first iteration of the internal EM algorithm.

  This method expects the \code{cases} argument to be a pathname
  pointing to a Netica cases file containing the training or test data (see
  \code{\link[RNetica]{write.CaseFile}}).  Also, it expects that there
  is a nodeset (see \code{\link[RNetica]{NetworkNodesInSet}}) attached
  to the network called \dQuote{onodes} which 
  references the observable variables in the case file.

  Before calling this method, the function \code{\link{BuildTable}} 
  needs to be called on each \code{Pnode} to both ensure that the
  conditional probability table is at a value reflecting the current
  parameters and to reset the value of
  \code{\link[RNetica]{NodeExperience}(node)} to the starting value
  of \code{\link{GetPriorWeight}(node)}.

  Note that Netica does allow
  \code{\link[RNetica]{NodeExperience}(node)} to have a different value
  for each row the the conditional probability table.  However, in this
  case, each node must have its own prior weight (or exactly the same
  number of parents).  The prior weight counts as a number of cases, and
  should be scaled appropriately for the number of cases in \code{cases}.  

  The parameters \code{Estepit} and \code{tol} are passed
  \code{\link[RNetica]{LearnCPTs}}.  Note that the outer EM
  algorithm assumes that the expected table counts given the current
  values of the parameters, so the default value of one is sufficient.
  (It is possible that a higher value will speed up convergence, the
  parameter is left open for experimentation.)  The tolerance is largely
  irrelevant as the outer EM algorithm does the tolerance test.

}
\value{

  The \code{net} argument is returned invisibly.

  As a side effect, the internal conditional probability tables in the
  network are updated as are the internal weights given to each row of
  the conditional probability tables.

}
\references{

  Almond, R. G. (2015) An IRT-based Parameterization for Conditional
  Probability Tables.  Paper presented at the 2015 Bayesian Application 
  Workshop at the Uncertainty in Artificial Intelligence Conference.

}
\author{Russell Almond}
\seealso{

  \code{\link[Peanut]{Pnet}}, \code{\link{Pnet.NeticaBN}},
  \code{\link[Peanut]{GEMfit}}, \code{\link[Peanut]{calcPnetLLike}},
  \code{\link[Peanut]{maxAllTableParams}},
  \code{\link[Peanut]{calcExpTables}},
  \code{\link[RNetica]{NetworkNodesInSet}}
  \code{\link[RNetica]{write.CaseFile}},
  \code{\link[RNetica]{LearnCPTs}}

}
\examples{
sess <- NeticaSession()
startSession(sess)

irt10.base <- ReadNetworks(system.file("testnets", "IRT10.2PL.base.dne",
                          package="PNetica"), session=sess)
irt10.base <- as.Pnet(irt10.base)  ## Flag as Pnet, fields already set.
irt10.theta <- NetworkFindNode(irt10.base,"theta")
irt10.items <- PnetPnodes(irt10.base)
## Flag items as Pnodes
for (i in 1:length(irt10.items)) {
  irt10.items[[i]] <- as.Pnode(irt10.items[[i]])
}
CompileNetwork(irt10.base) ## Netica requirement

casepath <- system.file("testdat","IRT10.2PL.200.items.cas",
                        package="PNetica")
## Record which nodes in the casefile we should pay attention to
NetworkNodesInSet(irt10.base,"onodes") <-
   NetworkNodesInSet(irt10.base,"observables")

item1 <- irt10.items[[1]]

priorcounts <- sweep(NodeProbs(item1),1,NodeExperience(item1),"*")

calcExpTables(irt10.base,casepath)

postcounts <- sweep(NodeProbs(item1),1,NodeExperience(item1),"*")

## Posterior row sums should always be larger.
stopifnot(
  all(apply(postcounts,1,sum) >= apply(priorcounts,1,sum))
)

DeleteNetwork(irt10.base)
stopSession(sess)

}
\keyword{ manip }

