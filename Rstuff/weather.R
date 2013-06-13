##new project! Weather

setwd("~/Desktop/")

## check argument for read.table to get variable names

x <- read.table("weather.txt", header = TRUE) ##C-c C-n, C-x o

##C-x C-f to find a doc/file

library(lubridate)
library(reshape)
library(ggplot2)
y <- melt(x, id="Year")

##y$index = 1:nrow(y)

stringDates <- paste(y$variable, y$Year, 20, sep="/")
Date <- as.Date(stringDates, format="%b/%Y/%d")
Temperatures <- y$value
data.frame(Date, Temperatures)
qplot(Date, Temperatures, geom = "line")
help("Sweave", package = "utils")
Sweave("weather.txt")
package.skeleton('SweaveTest')
