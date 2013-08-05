#' Makes a randPort object

#' Generates random portfolios for the evaluation of portfolio management

#' @param data A dataframe containing data about the universe of stocks
#' @param match.var Variables to match on
#' @param weight.var Weights for the stocks
#' @param ret.var The return variable
#' @param exposures A numeric vector of exposures to the matched factors the the output
#' portfolio should have, in the order they are given in match.var
#' @param replace FALSE if names in the original portfolio should not be in the new one
#' @param n Number of generated portfolios
#' @param verbose Set to TRUE to give verbose output
#' @param ... arguments to be passed to kmatching MCMC algorithms

#' @keywords Random-Portfolio

#' @export
#' @examples
#' data(jan)
#' ## this hangs, fix mirror
#' ##rP = randPort(data = jan, match.var = "growth", weight.var = "portfolio", ret.var = "fwd.ret.1m", n = 1000 )

randPort <- function(data, match.var=NULL, weight.var=NULL, ret.var = NULL, exposures=NULL, n, replace = TRUE,
                     verbose = FALSE, ...) {


    stopifnot(all(match.var %in% names(data)))
    if(!is.null(weight.var)) {
        stopifnot(length(weight.var) == 1)
        stopifnot(weight.var %in% names(data))
        if(all(data[[weight.var]] == 0)) {
            stop("All portfolio weights are set to zero")
        }
    }

    mW = kmatch(x = data, weight.var = weight.var, match.var = match.var, n=n, replace = replace, ...)
      
    if(is.null(match.var)) {
        match.var = character(0)
    }
    if(is.null(weight.var)){
        weight.var = character(0)
    }
    if(is.null(exposures)) {
        exposures = numeric(0)
    }
    if(is.null(ret.var)) {
      ret.var = character(0)
    }

    rP = new("randPort", match.var = match.var, weight.var =
    weight.var, exposures = exposures, matched.weights = mW, universe = data, x0provided = !is.null(weight.var))
    return(rP)
}
