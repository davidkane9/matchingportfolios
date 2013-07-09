\name{readWeather1}
\alias{readWeather1}
\title{Reads tables}
\usage{
  readWeather1(fileName, h, dateFormat)
}
\arguments{
  \item{fileName}{Name of the text file}

  \item{h}{Decides if the header is kept or not}

  \item{dateFormat}{The way that the dates are formatted}
}
\description{
  Creates a data frame with desirable data from data tables
  formatted the way it is in link 1.
}
\examples{
weather1 = readWeather1("weather.txt", TRUE, "\%b/\%Y/\%d")
}

