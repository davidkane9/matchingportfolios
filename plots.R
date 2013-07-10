library(devtools)
library(ggplot2)
## make sure you are in the package directory
load_all("randPort")

Amat = matrix(c(1,1,1), nrow = 1, ncol =3)
b = 1

## this sets the random number generator so we can have repeatable results
set.seed(40)

w = getWeights.hnr(A = Amat, b = b, n = 1000)
points = as.data.frame(t(w))
colnames(points) = c("GE", "IBM", "KO")

library(rgl)
scatterTheme1 = theme(
  panel.background = NULL,
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.title.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.text.x = element_blank(),
  plot.margin = unit(c(0.1,0.1,0.1,0.1), "lines")
)

scatterTheme2 = theme(
  panel.background = NULL,
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  plot.margin = unit(c(0.1,0.1,0.1,0.1), "lines")
)

scatterTheme3 = theme(
  panel.background = NULL,
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.title.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.text.y = element_blank(),
  plot.margin = unit(c(0.1,0.5,0.1,0.1), "lines")
)

histTheme1 = theme(
  panel.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  plot.margin = unit(c(0.1,0.05,0.1,1.5), "lines")
)

histTheme2 = theme(
  panel.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  plot.margin = unit(c(0.1,0.1,0.1,0.1), "lines")
)

histTheme3 = theme(
  panel.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  panel.border = element_blank(),
  axis.title.y = element_blank(),
  axis.ticks.y = element_blank(),
  axis.text.y = element_blank(),
  plot.margin = unit(c(0.1,0.1,0.1,0.1), "lines")
)

p1 <- ggplot(data = points, aes(x = GE, y = KO))
p1 <- p1 + geom_point(size = .1) + scatterTheme2

p2 <- ggplot(data = points, aes(x = GE, y = IBM))
p2 <- p2 + geom_point(size = .1) + scatterTheme1

p3 <- ggplot(data = points, aes(x = IBM, y = KO))
p3 <- p3 + geom_point(size = .1) + scatterTheme3

h1 <- ggplot(data = points, aes(x = GE))
h1 <- h1 + geom_histogram(binwidth = .125/4, fill = "white", color = "black")
h1 <- h1 + ylab("Counts") + histTheme1

h2 <- ggplot(data = points, aes(x = IBM))
h2 <- h2 + geom_histogram(binwidth = .125/4, fill = "white", color = "black")
h2 <- h2 + ylab("Counts") + histTheme2

h3 <- ggplot(data = points, aes(x = KO))
h3 <- h3 + geom_histogram(binwidth = .125/4, fill = "white", color = "black")
h3 <- h3 + ylab("Counts") + histTheme3

library(wq)
layOut(list(h1, 1, 1), list(p2, 2, 1), list(p1, 3, 1), list(h2, 2, 2), list(p3, 3, 2), list(h3, 3, 3))