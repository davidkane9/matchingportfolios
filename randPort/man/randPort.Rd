\name{randPort}
\alias{randPort}
\title{Makes a randPort object
Generates random portfolios for the evaluation of portfolio management}
\usage{
  randPort(data, match.var, weight.var, n)
}
\arguments{
  \item{data}{A dataframe containing data about the
  universe of stocks}

  \item{match.var}{Variables to match on}

  \item{weight.var}{Weights for the stocks}

  \item{n}{Number of generated portfolios}
}
\description{
  Makes a randPort object Generates random portfolios for
  the evaluation of portfolio management
}
\examples{
rP = randPort(jan, "value", "portfolio", 10)
}
\keyword{Random-Portfolio}

