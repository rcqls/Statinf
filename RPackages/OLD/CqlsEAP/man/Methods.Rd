\name{Methods Distribution}
\alias{Random}
\alias{Rand}
\alias{Sim}
\alias{Density}
\alias{Dens}
\alias{CumProb}
\alias{Prob}
\alias{Quantile}
\alias{Quant}
\title{Main methods of random distribution object}
\description{
  Since parametrizable random distribution use usual \R distribution,
  the corresponding methods are defined as \code{Random},
  \code{Density}, \code{CumProb} and \code{Quantile}. Shortname are
  associated for convenience for the user. In fact, the Random method
  acts also on others random objects (by instance, RandVar object. See
  example above).
}
\usage{
Random(Y,n)
Rand(Y,n)
Sim(Y,n)
Density(Y,x)
Dens(Y,x)
CumProb(Y,q)
Prob(Y,q)
Quantile(Y,p)
Quant(Y,p)
}
\arguments{
  \item{Y}{a random distribution object (or more generally a random
    object when acting only with Random method).}
  \item{n}{the number of realizations.}
  \item{x}{a real vector where the density is evaluated.}
  \item{q}{a real vector where the cumulative probability function is
    evaluated. }
  \item{p}{a real vector of probabilities}
}
\seealso{
  \code{\link{Distribution}} for the definition of parametrizable random
  distribution object, \code{\link{RandVar}} for the definition of
  random variable, \code{\link{Sample}} for the definition of sample object.
}
\examples{
Y=Normal() # N(0,1) by default
Random(Y,10) # or Rand(Y,10) or Sim(Y,10)
curve(Density(Y)(x),-4,4) # exactly what does plot(Y,-4,4)!!!
Prob(Y,seq(-5,5,l=10)) # CumProb(...) is larger but more explicit!!!
Quantile(Y,c(0,.25,.5,.75,1)) # Quant(...) is shorter!!!
Z=RandVar(Y^2)
mean(Sim(Z,100))
}
\keyword{random}
