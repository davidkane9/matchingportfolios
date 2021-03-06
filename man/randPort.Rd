\name{randPort}
\alias{randPort}
\title{Makes a randPort object
Generates random portfolios for the evaluation of portfolio management}
\usage{
  randPort(data, match.var = NULL, weight.var = NULL,
    ret.var = NULL, exposures = NULL, n, replace = TRUE,
    verbose = FALSE, ...)
}
\arguments{
  \item{data}{A dataframe containing data about the
  universe of stocks}

  \item{match.var}{Variables to match on}

  \item{weight.var}{Weights for the stocks}

  \item{ret.var}{The return variable}

  \item{exposures}{A numeric vector of exposures to the
  matched factors the the output portfolio should have, in
  the order they are given in match.var}

  \item{replace}{FALSE if names in the original portfolio
  should not be in the new one}

  \item{n}{Number of generated portfolios}

  \item{verbose}{Set to TRUE to give verbose output}

  \item{...}{arguments to be passed to kmatching MCMC
  algorithms}
}
\description{
  Makes a randPort object Generates random portfolios for
  the evaluation of portfolio management
}
\examples{
data(jan)
## this hangs, fix mirror
##rP = randPort(data = jan, match.var = "growth", weight.var = "portfolio", ret.var = "fwd.ret.1m", n = 1000 )
}
\keyword{Random-Portfolio}

