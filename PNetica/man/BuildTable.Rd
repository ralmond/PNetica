\name{BuildTable.NeticaNode}
\alias{BuildTable.NeticaNode}
\title{Builds the conditional probability table for a Pnode}
\description{

  The function \code{BuildTable} calls
  \code{\link[CPTtools]{calcDPCFrame}} to calculate the conditional
  probability for a \code{\link{Pnode}} object, and sets the current
  conditional probability table of \code{node} to the resulting value.
  It also sets the \code{\link[RNetica]{NodeExperience}(node)} to the
  current value of \code{\link[Peanut]{GetPriorWeight}(node)}.


}
\usage{
\method{BuildTable}{NeticaNode}(node)
}
\arguments{
  \item{node}{A \code{\link{Pnode.NeticaNode}} object whose table is to
    be built.} 

}
\details{

  The fields of the \code{\link{Pnode}} object correspond to the
  arguments of the \code{\link[CPTtools]{calcDPCTable}} function.  The
  output conditional probability table is then set in the node object in
  using the \code{[]} (\link[RNetica]{Extract.NeticaNode}) operator. 

  In addition to setting the CPT, the weight given to the nodes in the
  EM algorithm are set to \code{\link[Peanut]{GetPriorWeight}(node)},
  which will extract the value of
  \code{\link[Peanut]{PnodePriorWeight}(node)} or if that is null, the
  value of
  \code{\link[Peanut]{PnetPriorWeight}(\link[RNetica]{NodeParents}(node))}
  and set \code{\link[RNetica]{NodeExperience}(node)} to the resulting value.
  

}
\value{

  The \code{node} argument is returned invisibly.
  As a side effect the conditional probability table and experience of
  \code{node} is modified.

}
\references{

  Almond, R. G. (2015) An IRT-based Parameterization for Conditional
  Probability Tables.  Paper presented at the 2015 Bayesian Application 
  Workshop at the Uncertainty in Artificial Intelligence Conference.

}
\author{Russell Almond}
\seealso{
  \code{\link{Pnode.NeticaNode}}, \code{\link[Peanut]{Pnode}},
  \code{\link[Peanut]{PnodeQ}},
  \code{\link[Peanut]{PnodePriorWeight}}, \code{\link[Peanut]{PnodeRules}}, 
  \code{\link[Peanut]{PnodeLink}}, \code{\link[Peanut]{PnodeLnAlphas}}, 
  \code{\link[Peanut]{PnodeAlphas}}, \code{\link[Peanut]{PnodeBetas}}, 
  \code{\link[Peanut]{PnodeLinkScale}},\code{\link[Peanut]{GetPriorWeight}},
  \code{\link[CPTtools]{calcDPCTable}},
  \code{\link[RNetica]{NodeExperience}(node)}, 
  \code{\link[RNetica]{[.NeticaNode}} % ]  

}
\examples{
}
\keyword{ distribution }

