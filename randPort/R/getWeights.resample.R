#' Resamples weights
#'
#' Generates new weights by resampling
#'
#' @param weights Vector with weights
#' @param n Number of random portfolios generated
#'
#' @export
#' @examples
#'
#' weights = rep(0,20)
#' weights[1:5] = 1:5
#' getWeights.resample(weights, 10)

getWeights.resample <- function(weights, n) {
    weights.mat = matrix(sample(weights, length(weights)*n, replace =
    TRUE), ncol = n)
    weights.mat = apply(weights.mat, 2, function(c) c/sum(c))
    return(weights.mat)
}
