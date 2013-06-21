\name{getWeights.resample}
\alias{getWeights.resample}
\title{Resamples weights}
\usage{
  getWeights.resample(weights, n)
}
\arguments{
  \item{weights}{Vector with weights}

  \item{n}{Number of random portfolios generated}
}
\description{
  Generates new weights by resampling
}
\examples{
weights = rep(0,20)
weights[1:5] = 1:5
getWeights.resample(weights, 10)
}

