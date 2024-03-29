\name{PnetAdjoin}
\alias{PnetAdjoin,NeticaBN-method}
\alias{PnetDetach,NeticaBN-method}
\title{Merges (or separates) two Pnets with common variables}
\description{
  
  In the hub-and-spoke Bayes net construction method, number of spoke
  models (evidence models in educational applications) are connected to
  a central hub model (proficiency models in educational applications).
  The \code{PnetAdjoin} operation combines a hub and spoke model to make
  a motif, replacing references to hub variables in the spoke model with
  the actual hub nodes.  The \code{PnetDetach} operation reverses this. 


}
\usage{
\S4method{PnetAdjoin}{NeticaBN}(hub, spoke)
\S4method{PnetDetach}{NeticaBN}(motif, spoke) 
}
\arguments{
  \item{hub}{A complete \code{\link{Pnet}} to which new variables will
    be added. }
  \item{spoke}{An incomplete \code{\link{Pnet}} which may contain stub
    nodes, references to nodes in the \code{hub}}.
  \item{motif}{The combined \code{\link{Pnet}} which is formed by
    joining a hub and spoke together.}
}
\details{

  The hub-and-spoke model for Bayes net construction (Almond and
  Mislevy, 1999; Almond, 2017) divides a Bayes net into a central hub
  model and a collection of spoke models.  The motivation is that the
  hub model represents the status of a system---in educational
  applications, the proficiency of the student---and the spoke models
  are related to collections of evidence that can be collected about the
  system state.  In the educational application, the spoke models
  correspond to a collection of observable outcomes from a test item or
  task.  A \emph{motif} is a hub plus a collection of spoke model
  corresponding to a single task.

  While the hub model is a complete Bayesian network, the spoke models
  are fragments.  In particular, several hub model variables are parents
  of variables in the spoke model.  These variables are not defined in
  spoke model, but are rather replaced with \emph{stub nodes}, nodes
  which reference, but do not define the spoke model.

  The \code{PnetAdjoin} operation copies the \code{\link{Pnode}}s from
  the spoke model into the hub model, and connects the stub nodes to the
  nodes with the same name in the spoke model.  The result is a motif
  consisting of the hub and the spoke.  (If this operation is repeated
  many times it can be used to build an arbitrarily complex motif.)

  The \code{PnetDetach} operation reverses the adjoin operation.  It
  removes the nodes associated with the spoke model only, leaving the
  joint probability distribution of the hub model (along with any
  evidence absorbed by setting values of observable variables in the
  spoke) intact.

}
\value{

  The function \code{PnetAdjoin} returns a list of the newly created
  nodes corresponding to the spoke model nodes.  Note that the names may
  have changed to avoid duplicate names.  The names of the list are the
  spoke node names, so that any name changes can be discovered.

  In both cases, the first argument is destructively modified, for
  \code{PnetAdjoin} the hub model becomes the motif.  For
  \code{PnetDetach} the motif becomes the hub again.
  
}
\references{

    Almond, R. G. & Mislevy, R. J. (1999) Graphical models and computerized
    adaptive testing.  \emph{Applied Psychological Measurement}, 23,
    223--238. 

    Almond, R., Herskovits, E., Mislevy, R. J., & Steinberg,
    L. S. (1999). Transfer of information between system and evidence
    models. In Artificial Intelligence and Statistics 99, Proceedings
    (pp. 181--186). Morgan-Kaufman


  Almond, R. G. (presented 2017, August). Tabular views of Bayesian
  networks. In John-Mark Agosta and Tomas Singlair (Chair), \emph{Bayeisan
    Modeling Application Workshop 2017}. Symposium conducted at the
  meeting of Association for Uncertainty in Artificial Intelligence,
  Sydney, Australia. (International) Retrieved from
  \url{http://bmaw2017.azurewebsites.net/} 

}
\author{Russell Almond}
\section{Known Bugs}{

  Netica version 5.04 has a bug that when nodes with no graphical
  information (e.g., position) are absorbed in a net in which some of
  the nodes have graphical information, it will generate an error.  This
  was found and fixed in version 6.07 (beta) of the API.  However, the
  function \code{PnetDetach} may generate internal Netica errors in this
  condition.

  Right now they are logged, but nothing is done.  Hopefully, they are
  harmless.
  

}
\note{

  Node names must be unique within a Bayes net.  If several spokes are
  attached to a hub and those spokes have common names for observable
  variables, then the names will need to be modified to make them
  unique.  The function \code{PnetAdjoin} always returns the new nodes
  so that any name changes can be noted by the calling program.

  I anticipate that there will be considerable varation in how these
  functions are implemented depending on the underlying implementation
  of the Bayes net package.  In particular, there is no particular need
  for the \code{PnetDetach} function to do anything.  While removing
  variables corresponding to an unneeded spoke model make the network
  smaller, they are harmless as far as calculations of the posterior
  distribution.  

}
\seealso{

  \code{\link{Pnet}}, \code{\link{PnetHub}}, \code{\link{Qmat2Pnet}},
  \code{\link{PnetMakeStubNodes}}

}
\examples{

sess <- NeticaSession()
startSession(sess)

PM <- ReadNetworks(system.file("testnets", "miniPP-CM.dne",
                               package="PNetica"), session=sess)
EM1 <- ReadNetworks(system.file("testnets", "PPcompEM.dne",
                               package="PNetica"), session=sess)

Phys <- PnetFindNode(PM,"Physics")

## Prior probability for high level node
PnetCompile(PM)
bel1 <- PnodeMargin(PM, Phys)

## Adjoin the networks.
EM1.obs <- PnetAdjoin(PM,EM1)
PnetCompile(PM)

## Enter a finding
PnodeEvidence(EM1.obs[[1]]) <- "Right"
## Posterior probability for high level node

bel2 <- PnodeMargin(PM,Phys)

PnetDetach(PM,EM1)
PnetCompile(PM)

## Findings are unchanged
bel2a <- PnodeMargin(PM,Phys)
stopifnot(all.equal(bel2,bel2a,tol=1e-6))

DeleteNetwork(list(PM,EM1))
stopSession(sess)

}
\keyword{ graphs }
\keyword{ manip }
