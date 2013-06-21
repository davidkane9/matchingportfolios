\name{getWeights.shuffle}
\alias{getWeights.shuffle}
\title{Shuffles weights}
\usage{
  getWeights.shuffle(weights, n)
}
\arguments{
  \item{weights}{Vector with weights}

  \item{n}{Number of random portfolios generated}
}
\description{
  Assigns new indexes to all weighta
}
\examples{
weights = rep(0, 20)
weights[1:5] = 1:5
getWeights.shuffle(weights, 10)
}

