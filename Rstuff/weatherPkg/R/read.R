#' Reads tables
#'
#' Creates a data frame with desirable data from data tables formatted
#' the way they are in links 2 and 3.
#'
#' @param fileName Name of the text file
#' @param s Determines what the sep parameter in read.table() equals
#' @param xVar First variable for data frame
#' @param yVar Second variable for data frame
#'
#' @export

read <- function(fileName, s, xVar, yVar){
    if (s)
        dframe = read.table(fileName, sep = "\t")
    else
        dframe = read.table(fileName)
    cleanFrame = data.frame(dframe[[xVar]], dframe[[yVar]])
    colnames(cleanFrame)[1] = "Date"
    colnames(cleanFrame)[2] = "Temperature"
    return(cleanFrame)
}
