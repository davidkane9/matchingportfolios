library(devtools)
library(ggplot2)
## make sure you are in the package directory
load_all("randPort")

Amat = matrix(c(1,1,1), nrow = 1, ncol =3)
b = 1

## this sets the random number generator so we can have repeatable results
set.seed(40)

w = getWeights.hnr(A = Amat, b = b, n = 1000)

library(rgl)

d <- as.data.frame(t(w))
p1 <- ggplot(d, aes(w[1,], w[3,]))
p1 <- p1 + xlab("GE")
p1 <- p1 + ylab("KO")
p1 <- p1 + geom_point(size = .1)
##p1 <- p1 + theme(plot.margin = unit(0, "grobwidth"))
p1

p2 <- ggplot(d, aes(w[1,], w[2,]))
p2 <- p2 + xlab("GE")
p2 <- p2 + ylab("IBM")
p2 <- p2 + geom_point(size = .1)
p2

p3 <- ggplot(d, aes(w[2,], w[3,]))
p3 <- p3 + xlab("IBM")
p3 <- p3 + ylab("KO")
p3 <- p3 + geom_point(size = .1)
p3

h1 <- ggplot(d, aes(w[1,]))
h1 <- h1 + geom_histogram(binwidth = .125/4)
h1 <- h1 + xlab("GE")
h1 <- h1 + ylab("Counts")
h1

h2 <- ggplot(d, aes(w[2,]))
h2 <- h2 + geom_histogram(binwidth = .125/4)
h2 <- h2 + xlab("IBM")
h2 <- h2 + ylab("Counts")
h2

h3 <- ggplot(d, aes(w[3,]))
h3 <- h3 + geom_histogram(binwidth= .125/4)
h3 <- h3 + xlab("KO")
h3 <- h3 + ylab("Counts")
h3

library(wq)
layOut(list(h1, 1, 1), list(p2, 2, 1), list(p1, 3, 1), list(h2, 2, 2), list(p3, 3, 2), list(h3, 3, 3))