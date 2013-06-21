#' Makes a randPort object

#' Generates random portfolios for the evaluation of portfolio management

#' @param data A dataframe containing data about the universe of stocks
#' @param match.var Variables to match on
#' @param weight.var Weights for the stocks
#' @param n Number of generated portfolios

#' @keywords Random-Portfolio

#' @export
#' @examples
#'
#' rP = randPort(jan, "value", "portfolio", 10)

randPort <- function(data, match.var, weight.var, n) {


    stopifnot(all(match.var %in% names(data)))
    stopifnot(length(weight.var) == 1)
    stopifnot(weight.var %in% names(data))
    if(all(data[[weight.var]] == 0)) {
        stop("All portfolio weights are set to zero")
    }

    x0 = data[[weight.var]]
    Emat = matrix(1, ncol = nrow(data), nrow = 1)
    mW = getWeights(Emat, x0, n)
    rP = new("randPort", match.var = match.var, weight.var = weight.var, matched.weights = mW, universe = data)
    return(rP)
}
