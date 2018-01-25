\name{calcPnetLLike.NeticaBN}
\alias{calcPnetLLike.NeticaBN}
\title{Calculates the log likelihood for a set of data under a Pnet.NeticaBN model}
\description{

  The method \code{calcPnetLLike.NeticaBN} calculates the log likelihood for a
  set of data contained in \code{cases} using the current conditional
  probability tables in a \code{\link{Pnet.NeticaBN}}.  Here
  \code{cases} should be the filename of a Netica case file (see
  \code{\link[RNetica]{write.CaseFile}}).

}
\usage{
\method{calcPnetLLike}{NeticaBN}(net, cases)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{net}{A \code{\link{Pnet.NeticaBN}} object representing a
    parameterized network.} 
  \item{cases}{A character scalar giving the file name of a Netica case file
    (see \code{\link[RNetica]{write.CaseFile}}).}
}
\details{

  This function provides the convergence test for the
  \code{\link{GEMfit}} algorithm.  The \code{\link{Pnet.NeticaBN}}
  represents a model (with parameters set to the value used in the
  current iteration of the EM algorithm) and \code{cases} a set of data.
  This function gives the log likelihood of the data.

  This method expects the \code{cases} argument to be a pathname
  pointing to a Netica cases file containing the training or test data (see
  \code{\link[RNetica]{write.CaseFile}}).  Also, it expects that there
  is a nodeset (see \code{\link[RNetica]{NetworkNodesInSet}})
  attached to the network called \dQuote{onodes} which
  references the observable variables in the case file.

  As Netica does not have an API function to directly calculate the
  log-likelihood of a set of cases, this method loops through the cases
  in the case set and calls
  \code{\link[RNetica]{FindingsProbability}(net)} for each one.  Note
  that if there are frequencies in the case file, each case is weighted
  by its frequency.

}
\value{

  A numeric scalar giving the log likelihood of the data in the case
  file.

}
\references{

  Almond, R. G. (2015) An IRT-based Parameterization for Conditional
  Probability Tables.  Paper presented at the 2015 Bayesian Application 
  Workshop at the Uncertainty in Artificial Intelligence Conference.

}
\author{Russell Almond}
\seealso{

  \code{\link[Peanut]{Pnet}}, \code{\link{Pnet.NeticaBN}},
  \code{\link[Peanut]{GEMfit}}, \code{\link[Peanut]{calcExpTables}},
  \code{\link[Peanut]{BuildAllTables}}, \code{\link[Peanut]{maxAllTableParams}}
  \code{\link[RNetica]{NetworkNodesInSet}},
  \code{\link[RNetica]{FindingsProbability}},
  \code{\link[RNetica]{write.CaseFile}}

}
\examples{
sess <- NeticaSession()
startSession(sess)

irt10.base <- ReadNetworks(file.path(library(help="PNetica")$path,
                           "testnets","IRT10.2PL.base.dne"), session=sess)
irt10.base <- as.Pnet(irt10.base)  ## Flag as Pnet, fields already set.
irt10.theta <- NetworkFindNode(irt10.base,"theta")
irt10.items <- PnetPnodes(irt10.base)
## Flag items as Pnodes
for (i in 1:length(irt10.items)) {
  irt10.items[[i]] <- as.Pnode(irt10.items[[i]])
}
CompileNetwork(irt10.base) ## Netica requirement

casepath <- file.path(library(help="PNetica")$path,
                           "testdat","IRT10.2PL.200.items.cas")
## Record which nodes in the casefile we should pay attention to
NetworkNodesInSet(irt10.base,"onodes") <-
   NetworkNodesInSet(irt10.base,"observables")

llike <- calcPnetLLike(irt10.base,casepath)

DeleteNetwork(irt10.base)
stopSession(sess)


}
\keyword{ graphs }

