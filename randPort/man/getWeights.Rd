\name{getWeights}
\alias{getWeights}
\title{Generates weights to a new portfolio, randomly, and fulfilling
equality constraints.
Fulfills equality constraints while maintaining randomness by
using a Monte Carlo Random Walk.}
\usage{
  getWeights(Emat, x0, n, verbose = FALSE)
}
\arguments{
  \item{Emat}{This is the matrix of the equality constraint
  coefficients}

  \item{x0}{An original solution to the constraints}

  \item{n}{Number of random solutions to output}
}
\description{
  Generates weights to a new portfolio, randomly, and
  fulfilling equality constraints. Fulfills equality
  constraints while maintaining randomness by using a Monte
  Carlo Random Walk.
}
\examples{
Emat = matrix(1, ncol = 3, nrow = 1)
x0 = c(.3, .3, .4)
getWeights(Emat, x0, 1)
}
\keyword{Matching-Portfolio}
\keyword{Monte-Carlo}
\keyword{Random-Portfolio}

