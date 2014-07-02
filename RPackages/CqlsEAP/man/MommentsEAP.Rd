\name{Momments Methods by Simulation}
\alias{EspEAP}
\alias{VarEAP}
\alias{MommentEAP}
\alias{MthEAP}
\title{Momments methods for of Random Variables by Simulation (Experimental Approach of Probability)}
\description{
 Approximation of Esperance, Variance and Centred momments for random
 variable object using some large number of simulated realizations. 
}
\usage{
EspEAP(Y,m=.m.EAP)
VarEAP(Y,m=.m.EAP)
Momment(Y,k,m=.m.EAP)
Mth(Y,k,m=.m.EAP)
}
\arguments{
  \item{Y}{a random object.}
  \item{k}{order of the centred momment.}
  \item{m}{number of simulated realizations. The default value is .m.EAP=10000
    defined in the global environment.}
}
\seealso{
  \code{\link{Esp}},\code{\link{Var}} and \code{\link{Momment}}.
}
\examples{
Y=Binom(1,p) # p is not evaluated and could be defined after!!!
p=.2 
Esp(Y)
EspEAP(Y)
Var(Student(3))
VarEAP(Student(3),100000)
}
\keyword{random}
