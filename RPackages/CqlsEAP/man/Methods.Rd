\name{Methods Distribution}
\alias{Random}
\alias{sim}
\alias{Density}
\alias{ff}
\alias{CumProb}
\alias{FF}
\alias{Prob}
\alias{PP}
\alias{Quantile}
\alias{QQ}
\title{Main methods of random distribution object}
\description{
  Since parametrizable random distribution use usual \R distribution,
  the corresponding methods are defined as \code{Random} or \code{sim},
  \code{Density} or \code{ff}, \code{CumProb} or \code{FF}, and
  \code{Quantile} or \code{QQ}. 
}
\usage{
Random(Y,n)
sim(Y,n)
Density(Y,x)
ff(Y,x)
CumProb(Y,q)
FF(Y,q)
Quantile(Y,p)
QQ(Y,p)
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
  distribution object, \code{\link{Sample}} for the definition of sample object.
}
\examples{
Y<-Norm() # N(0,1) by default
sim(Y,10) # or Random(Y,10)
curve(ff(Y,x),-4,4) # exactly what does plot(Y,fr=-4,to=4)!!!
FF(Y,seq(-5,5,l=10))
QQ(Y,c(0,.25,.5,.75,1)) # Quant(...) is shorter!!!
Z<-Y^2
mean(sim(Z,100))
}
\keyword{random}
