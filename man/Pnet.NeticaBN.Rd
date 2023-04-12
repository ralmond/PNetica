\name{Pnet.NeticaBN}
\Rdversion{1.1}
\docType{class}
\alias{Pnet.NeticaBN}
\alias{PnetCompile,NeticaBN-method}
\alias{PnetDescription,NeticaBN-method}
\alias{PnetDescription<-,NeticaBN-method}
\alias{PnetHub,NeticaBN-method}
\alias{PnetHub<-,NeticaBN-method}
\alias{PnetName,NeticaBN-method}
\alias{PnetName<-,NeticaBN-method}
\alias{PnetTitle,NeticaBN-method}
\alias{PnetTitle<-,NeticaBN-method}
\alias{PnetDescription,NeticaBN-method}
\alias{PnetDescription<-,NeticaBN-method}
\alias{PnetPathname,NeticaBN-method}
\alias{PnetPathname<-,NeticaBN-method}
\alias{PnetPriorWeight,NeticaBN-method}
\alias{PnetPriorWeight<-,NeticaBN-method}
\alias{as.Pnet,NeticaBN-method}
\alias{is.Pnet,NeticaBN-method}

\title{Class \code{"NeticaBN"} as a \code{"Pnet"}}
\description{

  The \code{PNetica} package supplies the needed methods so that the
  \code{RNetica::\linkS4class{NeticaBN}} object is an instance of the
  \code{Peanut::\linkS4class{Pnet}} object.

}
\section{Extends}{

  See \code{\linkS4class{NeticaBN}} for a description of the Netica class.

  With these methods, \code{\linkS4class{NeticaBN}} now extends
  \code{\linkS4class{Pnet}}. 


  All reference classes extend and inherit methods from
  \code{"\linkS4class{envRefClass}"}. 

}
\section{Methods}{
  \describe{
    \item{\link[Peanut]{PnetCompile}}{\code{signature(net =
	"NeticaBN")}: Compiles the network. }
    \item{\link[Peanut]{PnetName}}{\code{signature(net = NeticaBN)}:
      Gets the name of the network.}
    \item{\link[Peanut]{PnetName<-}}{\code{signature(net = NeticaBN)}:
      Sets the name of the network.}
    \item{\link[Peanut]{PnetTitle}}{\code{signature(net = NeticaBN)}:
      Gets the title of the network.}
    \item{\link[Peanut]{PnetTitle<-}}{\code{signature(net = NeticaBN)}:
      Sets the title of the network.}
    \item{\link[Peanut]{PnetDescription}}{\code{signature(net =
	NeticaBN)}: Gets the description of the network.}
    \item{\link[Peanut]{PnetDescription<-}}{\code{(signature(net =
	NeticaBN)}:  Sets the description of the network.}
    \item{\link[Peanut]{PnetPathname}}{\code{signature(net = NeticaBN)}:
      Gets the pathname where the network is stored.}
    \item{\link[Peanut]{PnetPathname<-}}{\code{signature(net =
	NeticaBN)}:  Sets the pathname where the network is stored.}
    \item{\link[Peanut]{PnetHub}}{\code{signature(net = NeticaBN)}:
      Returns the name of the hub (competency/proficiency model)
      associated with an spoke (evidence model) network.}
    \item{\link[Peanut]{PnetHub<-}}{\code{signature(net = NeticaBN)}:
      Sets the name of the hub.}
    \item{\link[Peanut]{PnetPriorWeight}}{\code{signature(net = NeticaNode)}:
      Returns the default prior weight 
      associated with nodes in this network.}
    \item{\link[Peanut]{PnetPriorWeight<-}}{\code{signature(net = NeticaNode)}:
      Sets the default prior weight 
      associated with nodes in this network.}
    \item{\link[Peanut]{as.Pnet}}{\code{signature(x = NeticaBN)}:
      Forces \code{x} to be a \code{\linkS4class{Pnet}}.}
    \item{\link[Peanut]{is.Pnet}}{\code{signature(x = NeticaBN)}:
      Returns true.}
  }
}
\author{Russell Almond}
\seealso{

  Base class:  \code{\linkS4class{NeticaBN}}.

  Mixin class: \code{\linkS4class{Pnet}}.

  Methods (from \code{Peanut} package.):

  \code{\link[Peanut]{PnetCompile}},
  \code{\link[Peanut]{PnetHub}},
  \code{\link[Peanut]{PnetName}},
  \code{\link[Peanut]{PnetTitle}},
  \code{\link[Peanut]{PnetDescription}},
  \code{\link[Peanut]{PnetPathname}},
  \code{\link[Peanut]{as.Pnet}},
  \code{\link[Peanut]{is.Pnet}}.
  
}
\examples{
sess <- NeticaSession()
startSession(sess)
curd <- setwd(system.file("testnets",package="PNetica"))

## PnetHub
PM <- ReadNetworks("miniPP-CM.dne", session=sess)
stopifnot(PnetHub(PM)=="")

EM1 <- ReadNetworks("PPcompEM.dne", session=sess)
stopifnot(PnetHub(EM1)=="miniPP_CM")

foo <- CreateNetwork("foo",sess)
stopifnot(is.na(PnetHub(foo)))
PnetHub(foo) <- PnetName(PM)
stopifnot(PnetHub(foo)=="miniPP_CM")

## PnetCompile
PnetCompile(PM)
marginPhysics <- Statistic("PnodeMargin","Physics","Pr(Physics)")
calcStat(marginPhysics,PM)

net <- CreateNetwork("funNet",sess)
stopifnot(PnetName(net)=="funNet")

PnetName(net)<-"SomethingElse"
stopifnot(PnetName(net)=="SomethingElse")

## PnetPathname
stopifnot(PnetPathname(PM)=="miniPP-CM.dne")
PnetPathname(PM) <- "StudentModel1.dne"
stopifnot(PnetPathname(PM)=="StudentModel1.dne")

##PnetTitle and PnetDescirption
firstNet <- CreateNetwork("firstNet",sess)

PnetTitle(firstNet) <- "My First Bayesian Network"
stopifnot(PnetTitle(firstNet)=="My First Bayesian Network")

now <- date()
PnetDescription(firstNet)<-c("Network created on",now)
## Print here escapes the newline, so is harder to read
cat(PnetDescription(firstNet),"\n")
stopifnot(PnetDescription(firstNet) ==
  paste(c("Network created on",now),collapse="\n"))



DeleteNetwork(list(PM,EM1,foo,net,firstNet))
stopSession(sess)
setwd(curd)

}
\keyword{classes}
\keyword{graphs}
