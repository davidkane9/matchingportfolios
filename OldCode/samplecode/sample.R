library(devtools)
## make sure you are in the package directory
load_all()

Amat = matrix(c(1,1,1), nrow = 1, ncol =3)
b = 1

## this sets the random number generator so we can have repeatable results
set.seed(40)

w = getWeights.hnr(A = Amat, b = b, n = 1000)
points = as.data.frame(t(w))
colnames(points) = c("GE", "IBM", "KO")

library(rgl)
plot3d(w[1,], w[2,], w[3,])

library(ggplot2)
dat = data.frame(x = 1:10, y = 1:10)

g = ggplot(data = points, aes(x=GE, y=IBM))
g + geom_point()
g+ geom_point(size = 1) + scatterTheme
g + geom_point() + histTheme

scatterTheme = theme(
  panel.background = NULL,
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank(),
  plot.margin = unit(c(0,0,0,0), "lines"),
  panel.margin = unit(c(0,0,0,0), "lines")
  )

histTheme = theme(
  panel.background = element_blank(),
  panel.grid.major = element_blank(),
  panel.grid.minor = element_blank(),
  axis.title = element_blank(),
  axis.ticks = element_blank(),
  axis.text = element_blank()
  )