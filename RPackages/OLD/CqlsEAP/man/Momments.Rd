\name{Momments Methods}
\alias{Esp}
\alias{Var}
\alias{Momment}
\alias{Mth}
\title{Momments methods for of Random Variables}
\description{
 Esperance, Variance and Centred momments for random variable object. 
}
\usage{
Esp(Y)
Var(Y)
Momment(Y,k)
Mth(Y,k)
}
\arguments{
  \item{Y}{a random object.}
  \item{k}{order of the centred momment.}
}
\seealso{
  \code{\link{EspEAP}},\code{\link{VarEAP}} and \code{\link{MommentEAP}}
  for the corresponding methods approximating \code{Esp}, \code{Var} and
  \code{Momment}  by simulation.
}
\examples{
Y=Binom(1,p) # p is not evaluated and could be defined after!!!
p=.2 
Esp(Y)
Var(Y)
Momment(Y,3) # or shortly Mth(Y,3)
}
\keyword{random}
