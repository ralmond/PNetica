\name{PnodeParentTvals.NeticaNode}
\alias{PnodeParentTvals.NeticaNode}
\title{Fetches a list of numeric variables corresponding to parent states}
\description{

  In constructing a conditional probability table using the discrete
  partial credit framework (see \code{\link[CPTtools]{calcDPCTable}}),
  each state of each parent variable is mapped onto a real value called
  the effective theta.  The  \code{PnodeParentTvals} method for Netica
  nodes returns the result of applying \code{\link[RNetica]{NodeLevels}}
  to each of the nodes in \code{\link[RNetica]{NodeParents}(node)}. 

}
\usage{
\method{PnodeParentTvals}{NeticaNode}(node)
}
\arguments{
  \item{node}{A \code{\link{Pnode}} which is also a
    \code{\link[RNetica]{NeticaNode}}. }
}
\details{

  While the best practices for assigning values to the states of the
  parent nodes is probably to assign equal spaced values (using the
  function \code{\link[CPTtools]{effectiveThetas}} for this purpose),
  this method needs to retain some flexibility for other possibilities.
  However, in general, the best choice should depend on the meaning of
  the parent variable, and the same values should be used everywhere the
  parent variable occurs.

  Netica already provides the \code{\link[RNetica]{NodeLevels}} function
  which allows the states of a \code{\link[RNetica]{NeticaNode}} to be
  associated with numeric values.  This method merely gathers them
  together.  The method assumes that all of the parent variables have
  had their \code{\link[RNetica]{NodeLevels}} set and will generate an
  error if that is not true.

}
\value{

  \code{PnodeParentTvals(node)} should return a list corresponding to
  the parents of \code{node}, and each element should be a numeric
  vector corresponding to the states of the appropriate parent
  variable.  If there are no parent variables, this will be a list of no
  elements.    

}
\references{

  Almond, R. G. (2015) An IRT-based Parameterization for Conditional
  Probability Tables.  Paper presented at the 2015 Bayesian Application 
  Workshop at the Uncertainty in Artificial Intelligence Conference.

  Almond, R.G., Mislevy, R.J., Steinberg, L.S., Williamson, D.M. and
  Yan, D. (2015) \emph{Bayesian Networks in Educational Assessment.}
  Springer.  Chapter 8.

}
\author{Russell Almond}
\note{

  The implementation is merely:
  \code{lapply(NodeParents(node), NodeLevels)}.

}
\seealso{
       \code{\link{Pnode.NeticaNode}}, \code{\link[Peanut]{Pnode}}, 
       \code{\link[CPTtools]{effectiveThetas}},
       \code{\link{BuildTable.NeticaNode}},
       \code{\link{maxCPTParam.NeticaNode}}
}
\examples{
tNet <- CreateNetwork("TestNet")

theta1 <- NewDiscreteNode(tNet,"theta1",
                         c("VH","High","Mid","Low","VL"))
## This next function sets the effective thetas for theta1
NodeLevels(theta1) <- effectiveThetas(NodeNumStates(theta1))
NodeProbs(theta1) <- rep(1/NodeNumStates(theta1),NodeNumStates(theta1))
theta2 <- NewDiscreteNode(tNet,"theta2",
                         c("High","Mid","Low"))
## This next function sets the effective thetas for theta2
NodeLevels(theta2) <- effectiveThetas(NodeNumStates(theta2))
NodeProbs(theta2) <- rep(1/NodeNumStates(theta2),NodeNumStates(theta2))

partial3 <- NewDiscreteNode(tNet,"partial3",
                            c("FullCredit","PartialCredit","NoCredit"))
NodeParents(partial3) <- list(theta1,theta2)

## Usual way to set rules is in constructor
partial3 <- Pnode(partial3,rules="Compensatory", link="partialCredit")

PnodeParentTvals(partial3)
do.call("expand.grid",PnodeParentTvals(partial3))

DeleteNetwork(tNet)
}
\keyword{ attrib }

