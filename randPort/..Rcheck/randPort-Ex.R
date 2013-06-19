pkgname <- "randPort"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
options(pager = "console")
library('randPort')

assign(".oldSearch", search(), pos = 'CheckExEnv')
cleanEx()
nameEx("getWeights")
### * getWeights

flush(stderr()); flush(stdout())

### Name: getWeights
### Title: Generates weights to a new portfolio, randomly, and fulfilling
###   equality constraints. Fulfills equality constraints while maintaining
###   randomness by using a Monte Carlo Random Walk.
### Aliases: getWeights
### Keywords: Matching-Portfolio Monte-Carlo Random-Portfolio

### ** Examples

Emat = matrix(1, ncol = 3, nrow = 1)
x0 = c(.3, .3, .4)
getWeights(Emat, x0, 1)



### * <FOOTER>
###
cat("Time elapsed: ", proc.time() - get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
