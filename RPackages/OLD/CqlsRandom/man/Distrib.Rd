\name{Distribution}
\alias{Distribution}
\alias{Distrib}
\alias{Uniform}
\alias{Unif} 
\alias{Normal}
\alias{Norm}
\alias{Binom}
\alias{ChooseIn}
\alias{Discrete}
\alias{Student}
\alias{St}
\alias{Chisquare}
\alias{Chi2}
\alias{Fisher}
\title{Parametrizable Random Distribution}
\description{
  Another way to use random distribution. One could use unevaluated
  parameters (see example).
}
\usage{
Distribution(name,...)
Unif(min,max)
Norm(mean,sd)
Binom(size,prob)
Discrete(mod,prob)
Chi2(df,ncp)
Fisher(df1,df2)
...
}
\arguments{
  \item{name}{a distribution name (i.e. "norm","chisq",...).}
  \item{...}{the parameters of the distribution.}
}
\seealso{
  Methods \code{\link{Random}}, \code{\link{CumProb}},
  \code{\link{Density}} and \code{\link{Quantile}} corresponding to the
  "r","p","d" and "q" methods of usual \R distribution.
  Methods \code{\link{Mean}}, \code{\link{Var}}, \code{\link{Moment}}
  providing Esperance, Variance and centred Momment of Distribution.
}
\examples{
Y<-Binom(1,p) # p is not evaluated and could be defined later!!!
p<-.2 
sim(Y,10) # rnorm(10,1,.2) since p=.2
EE(Y)       # or Mean(Y)
VV(Y)      # or Var(Y)
MM(Y,3) # or Moment(Y,3)
p<-.3
# dynamic tricks!!!
sim(Y,10) # rnorm(10,1,.3) since p=.3
EE(Y)
}
\keyword{random}
