#' Reads tables
#'
#' Creates a data frame with desirable data from data tables formatted
#' the way it is in link 1.
#'
#' @param fileName Name of the text file
#' @param h Decides if the header is kept or not
#'
#' @export

readWeather1 <- function(fileName, h) {
	library(reshape)
    dframe = read.table(fileName, header = h)
    mframe = melt(dframe, id = "Year")
    stringDates <- paste(mframe[,1],mframe[,2],mframe[,3], sep = "\t")
    Date = as.Date(stringDates, format="%b/%Y/%d")
    Temperature <- mframe$value
    cleanFrame <- data.frame(Date, Temperature)
    return(cleanFrame)
}
