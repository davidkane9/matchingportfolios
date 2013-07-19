library(devtools)
library(ggplot2)
## make sure you are in the package directory
load_all("randPort")

Amat = matrix(c(1,1,1), nrow = 1, ncol =3)
Amat = rbind(Amat, c(1, 0, -1))
b = c(1, 0)

## this sets the random number generator so we can have repeatable results
set.seed(40)

w = getWeights.hnr(A = Amat, b = b, n = 1000)
points = as.data.frame(t(w))
colnames(points) = c("GE", "IBM", "KO")

points2d = data.frame(x = 1*points$IBM + 1/2*points$KO, y = sqrt(3)/2*points$KO)

simplexTheme = theme(
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  plot.margin = unit(c(0.1,0.1,0.1,0.1), "lines")
)

g = ggplot(data = points2d, aes(x,y))
g = g + geom_point()
g = g + simplexTheme
g = g + geom_segment(aes(x = 0, y = 0, xend = .5, yend = sqrt(3)/2))
g = g + geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0))
g = g + geom_segment(aes(x = .5, y = sqrt(3)/2), xend = 1, yend = 0)

png("simplex2.png")
g
dev.off()