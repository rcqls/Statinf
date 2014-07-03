\name{Plot of random distribution object}
\alias{plot.distrib}
\title{Plot of random distribution object}
\description{
  For convenience, the plot method of some random distribution object
  return the curve of the density function (i.e. only implemented for
  continuous distribution).
}
\usage{
plot.distrib(Y,...)
plot(Y,...)
}
\arguments{
  \item{Y}{a random (continuous) distribution object.}
  \item{...}{arguments passed through \code{curve} function.}
}
\seealso{
  \code{\link{Distribution}} for the definition of parametrizable random.
}
\examples{
plot(Normal(),-4,4)
}
\keyword{random}
