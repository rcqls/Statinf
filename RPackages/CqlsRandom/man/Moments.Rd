\name{Momments Methods}
\alias{Mean}
\alias{EE}
\alias{Var}
\alias{VV}
\alias{Momment}
\alias{MM}
\title{Momments methods for of Random Variables}
\description{
 Esperance, Variance and Centred momments for random variable object. 
}
\usage{
EE(Y)
VV(Y)
MM(Y,k)
}
\arguments{
  \item{Y}{a random object.}
  \item{k}{order of the centred momment.}
}
\seealso{
  \code{\link{EE.EAP}},\code{\link{VV.EAP}} and \code{\link{MM.EAP}}
  for the corresponding methods approximating \code{EE}, \code{VV} and
  \code{MM}  by simulation.
}
\examples{
Y<-Binom(1,p) # p is not evaluated and could be defined after!!!
p<-.2 
EE(Y)
VV(Y)
MM(Y,3)
}
\keyword{random}
