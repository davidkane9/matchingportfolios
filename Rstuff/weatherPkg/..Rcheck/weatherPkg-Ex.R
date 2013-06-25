pkgname <- "weatherPkg"
source(file.path(R.home("share"), "R", "examples-header.R"))
options(warn = 1)
library('weatherPkg')

base::assign(".oldSearch", base::search(), pos = 'CheckExEnv')
cleanEx()
nameEx("readWeather1")
### * readWeather1

flush(stderr()); flush(stdout())

### Name: readWeather1
### Title: Reads tables
### Aliases: readWeather1

### ** Examples

weather1 = readWeather1("weather.txt", TRUE, "%b/%Y/%d")



cleanEx()
nameEx("weatherPkg-package")
### * weatherPkg-package

flush(stderr()); flush(stdout())

### Name: weatherPkg-package
### Title: What the package does (short line) ~~ package title ~~
### Aliases: weatherPkg-package weatherPkg
### Keywords: package

### ** Examples

~~ simple examples of the most important functions ~~



### * <FOOTER>
###
options(digits = 7L)
base::cat("Time elapsed: ", proc.time() - base::get("ptime", pos = 'CheckExEnv'),"\n")
grDevices::dev.off()
###
### Local variables: ***
### mode: outline-minor ***
### outline-regexp: "\\(> \\)?### [*]+" ***
### End: ***
quit('no')
