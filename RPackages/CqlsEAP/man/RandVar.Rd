\name{Random Variable Expression}
\alias{RandVar}
\alias{RV}
\alias{RandExp}
\title{Random Variable Expression}
\description{
  Random variable expression could be defined by using basic random
  distribution objects or another random variable expression. 
}
\usage{
RandVar(...)
RV(...)
RandExp(...)
}
\arguments{
  \item{...}{expression with any random object (i.e. responding to the Random method).}
}
\details{
  \code{RandExp} is similar to \code{RandVar} (shortly named \code{RV})
  but more general in the sense that the result could be list of
  vectors. However, \code{RandVar} is really faster than \code{RandExp}
  when Random(...,n) (with n>>1) is called since
  Random(...,n) for random objects inside the random expression are used
  instead of Random(...,1) combined with some lapply call.
  
}
\seealso{
  \code{\link{Distribution}} for the definition of parametrizable random
  distribution object, \code{\link{Sample}} for the definition of sample object.
}
\note{
  Until now, random variable in RandVar or RandExp have to assigned to
  name variable (i.e. \code{RV(Norm(0,1)^2+Norm(0,1)^2)} is not well-suited).
  This will be some nice improvement in future version.
}
\examples{
Y=Normal() # N(0,1) by default
Y2=Normal()
Z=RV(Y^2+Y2^2) # by default, Y and Y2 are independent!!!
Sim(Z,10) # or Random(Z,10)
Esp(Z) # in fact, EspEAP(Z)
Esp(Chi2(2)) # True esperance since Z is a Chisquare(2) distribution!!!
}
\keyword{random}
