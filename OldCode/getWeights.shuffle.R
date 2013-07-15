#' Shuffles weights
#'
#' Assigns new indexes to all weighta
#'
#' @param weights Vector with weights
#' @param n Number of random portfolios generated
#'
#' @export
#' @examples
#'
#' weights = rep(0, 20)
#' weights[1:5] = 1:5
#' getWeights.shuffle(weights, 10)

getWeights.shuffle <- function(weights, n) {
    weights.list = NULL
    w = weights[which(weights > 0)]
    for(i in 1:n){
        indexes = sample(1:length(weights), length(w))
        newcol = matrix(0, nrow = length(weights), ncol = 1)
        newcol[indexes,] = w
        weights.list = c(weights.list, list(col = newcol))
    }
    weights.mat = do.call(cbind, weights.list)
    return(weights.mat)
}
