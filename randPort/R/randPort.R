#' Makes a randPort object

#' Generates random portfolios for the evaluation of portfolio management

#' @param data A dataframe containing data about the universe of stocks
#' @param match.var Variables to match on
#' @param weight.var Weights for the stocks
#' @param n Number of generated portfolios
#' @param type Type of random sampling (shuffle, resample, reflect)

#' @keywords Random-Portfolio

#' @export
#' @examples
#'
#' rP = randPort(jan, "value", "portfolio", 10)

randPort <- function(data, match.var, weight.var, n, type) {


    stopifnot(all(match.var %in% names(data)))
    stopifnot(length(weight.var) == 1)
    stopifnot(weight.var %in% names(data))
    if(all(data[[weight.var]] == 0)) {
        stop("All portfolio weights are set to zero")
    }
    x0 = data[[weight.var]]
    if(type == "reflect") {
        Emat = matrix(1, ncol = nrow(data), nrow = 1)
        mW = getWeights(Emat, x0, n)
    } else if(type == "resample") {
        mW = getWeights.resample(x0, n)
    } else if(type == "shuffle") {
        mW = getWeights.shuffle(x0, n)
    } else if(type == "hnr") {
        Amat = matrix(1, ncol = nrow(data), nrow = 1)
        for(v in match.var) {
            Amat = rbind(Amat, data[[v]])
        }
        x0 = data[[weight.var]]
        mW = getWeights.hnr(Amat, x0, n, 10)
    } else {
        stop("Invalid type")
    }
    rP = new("randPort", match.var = match.var, weight.var =
    weight.var, matched.weights = mW, universe = data)
    return(rP)
}
