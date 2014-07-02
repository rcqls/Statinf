\name{Sample of Random object}
\alias{Sample}
\title{Sample of Random object}
\description{
  When some univariate random object is defined, Sample provides some
  random sample object. Usual fonction like \code{mean}, \code{var} and
  \code{sd} could be applied on this random sample to define some new
  univariate random object.
}
\usage{
Sample(Y,n)
}
\arguments{
  \item{Y}{an univariate random object.}
  \item{n}{the sample size. Notice that n could be any name variable
    evaluated later but before any Random method call.}
}
\seealso{
  \code{\link{Distribution}} for the definition of parametrizable random
  distribution object.
}
\examples{
Y<-Binom(1,p) # p is not evaluated and could be defined after!!!
p<-.2
YY<-Sample(Y,n) # n defined after!!!
n<-100
sim(YY,10) # the result is a list!!!
Est<-mean(YY) # random sample mean of Y
sim(Est,5) # the result is a vector of length 5!!!
EE(Est) # EE.EAP(Est) is used instead (of course) !!! (Be patient!!!)
}
\keyword{random}
