\name{Methods by Simulation}
\alias{Mean.EAP}
\alias{EE.EAP}
\alias{EE.sim}
\alias{Var.EAP}
\alias{VV.EAP}
\alias{VV.sim}
\alias{Moment.EAP}
\alias{MM.EAP}
\alias{MM.sim}
\title{Methods for of Random Variables by Simulation (Experimental Approach of Probability)}
\description{
 Approximation of Esperance, Variance and Centred momments for random
 variable object using some large number of simulated realizations. 
}
\usage{
EE.EAP(Y,m=.m.EAP)
VV.EAP(Y,m=.m.EAP)
MM.EAP(Y,k,m=.m.EAP)
}
\arguments{
  \item{Y}{a random object.}
  \item{k}{order of the centred momment.}
  \item{m}{number of simulated realizations. The default value is .m.EAP=10000
    defined in the global environment.}
}
\seealso{
  \code{\link{Mean}},\code{\link{Var}} and \code{\link{Moment}}.
}
\examples{
Y<-Binom(1,p) # p is not evaluated and could be defined after!!!
p<-.2 
EE(Y)
EE.EAP(Y)
Var(Student(3))
Var.EAP(Student(3),100000)
}
\keyword{random}
