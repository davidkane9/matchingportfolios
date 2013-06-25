#' Reads tables
#'
#' Creates a data frame with desirable data from data tables formatted
#' the way it is in link 1.
#'
#' @param fileName Name of the text file
#' @param h Decides if the header is kept or not
#' @param dateFormat The way that the dates are formatted
#'
#' @export
#' @examples
#'
#' weather1 = readWeather1("weather.txt", TRUE, "%b/%Y/%d")

readWeather1 <- function(fileName, h, dateFormat) {
    dframe = read.table(fileName, header = h)
    mframe = melt(dframe, id = "Year")
    stringDates <- paste(mframe, sep = "/")
    Date = as.Date(stringDates, format=dateFormat)
    Temperature <- mframe$value
    cleanFrame <- data.frame(Date, Temperature)
    return(cleanFrame)
}
