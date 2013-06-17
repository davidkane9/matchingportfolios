
#' Loads weather data from Williamstown
#'
#' This data is taken once every month from 1892 to 2010, as opposed to
#' the others which are taken over smaller periods but have
#' higher resolution.
#'
#' @keywords Williamstown
#'
#' @example
#' y = loadWeather()
#' head(y)

loadWeather <- function() {
    x <- read.table("weather.txt", header = TRUE)
    y <- melt(x, id="Year", )
    stringDates <- paste(y$variable, y$Year, 20, sep="/")
    y$Date <- as.Date(stringDates, format="%b/%Y/%d")
    y$Temperatures <- y$value
    return(y)
}
