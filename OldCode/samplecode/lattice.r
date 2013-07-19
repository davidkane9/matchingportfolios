library(rgl)
library(scatterplot3d)

A = matrix(1, ncol =4, nrow = 1)
b = 1
w = hitandrun(A, b, n = 1000)
points = data.frame(a = w[1,], b = w[2,], c= w[3,], d=w[4,])

s = w[,1:3]
g = ggplot(data = points, aes(x = a, y= b)) + geom_point()

rgl.surface(s[1,], s, s[3,])


data(volcano)

y <- 2 * volcano        # Exaggerate the relief

x <- 10 * (1:nrow(y))   # 10 meter spacing (S to N)
z <- 10 * (1:ncol(y))   # 10 meter spacing (E to W)

ylim <- range(y)
ylen <- ylim[2] - ylim[1] + 1

colorlut <- terrain.colors(ylen) # height color lookup table

col <- colorlut[ y-ylim[1]+1 ] # assign colors to heights for each point

rgl.open()
rgl.surface(x, z, y, color=col, back="lines")


library(lattice)
dat = expand.grid( x = 0:10, y = 0:10)
dat$z = 1
wireframe(z ~ x + y, data = dat, shade = TRUE )

kx = function(u, v) 
  u
ky = function(u, v) 
  v
kz = function(u, v) 
  1-u-v
n = 50
u = seq(-10,10, length = n)
v = seq(-10,10,length = n)
um = matrix(u, length(u), length(u))

vm = matrix(v, length(v), length(v), byrow = TRUE)
r = 2
t = 1
wireframe(kz(um, vm) ~ kx(um, vm) + ky(um, vm), shade = FALSE, screen = list(z = 170, x =-60),
          alpha = 0.75, panel.aspect = 0.6, aspect = c(1, 0.4))