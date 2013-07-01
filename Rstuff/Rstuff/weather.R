##new project! Weather

setwd("~/Desktop/WilliamstownWeather/RStuff")

## check argument for read.table to get variable names

x <- read.table("MonthlyTemp.txt", header = TRUE) ##C-c C-n, C-x o

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

##code for 2nd and 3rd graphs

weather2 <- read.table("DailyTemp1.txt")
dates1 <- weather2[,1]
temps1 <- weather2[,3]
plot(dates1, temps1)

weather3 <- read.table("DailyTemp2.txt", sep = "\t")
dates2 <- weather3[,1]
temps2 <- weather3[,2]
plot(dates2, temps2)
