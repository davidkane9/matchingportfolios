loadWeather <-
function() {
    x <- read.table("weather.txt", header = TRUE)
    y <- melt(x, id="Year", )
    stringDates <- paste(y$variable, y$Year, 20, sep="/")
    y$Date <- as.Date(stringDates, format="%b/%Y/%d")
    y$Temperatures <- y$value
    return(y)
}
