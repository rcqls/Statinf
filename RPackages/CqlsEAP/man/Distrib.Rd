\name{Distribution}
\alias{Distribution}
\alias{Normal}
\alias{Norm}
\alias{Binom}
\alias{Discrete}
\alias{Student}
\alias{St}
\title{Parametrizable Random Distribution}
\description{
  Another way to use random distribution. One could use unevaluated
  parameters (see example).
}
\usage{
Distribution(name,...)
Normal(mean,sd)
Binom(size,prob)
Discrete(mod,prob)
Chi2(df,ncp)
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
  Methods \code{\link{Esp}}, \code{\link{Var}}, \code{\link{Momment}}
  providing Esperance, Variance and centred Momment of Distribution.
}
\examples{
Y=Binom(1,p) # p is not evaluated and could be defined after!!!
p=.2 
Random(Y,10) # rnorm(10,1,.2) since p=.2
Esp(Y)
Var(Y)
Momment(Y,3) # or shortly Mth(Y,3)
p=.3
Random(Y,10) # rnorm(10,1,.3) since p=.3
}
\keyword{random}
