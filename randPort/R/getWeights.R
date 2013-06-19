#' Generates weights to a new portfolio, randomly, and fulfilling
#' equality constraints.

#' Fulfills equality constraints while maintaining randomness by
#' using a Monte Carlo Random Walk.

#' @param Emat This is the matrix of the equality constraint coefficients
#' @param x0 An original solution to the constraints
#' @param n Number of random solutions to output

#' @keywords Random-Portfolio Matching-Portfolio Monte-Carlo

#' @export
#' @examples
#' Emat = matrix(1, ncol = 3, nrow = 1)
#' x0 = c(.3, .3, .4)
#' getWeights(Emat, x0, 1)
getWeights <- function(Emat, x0, n) {
    Z = Null(t(Emat))
    ret = matrix(0, nrow = length(x0), ncol = n + 1)
    ## Would it be better to use apply here?
    nc = ncol(Z)
    mn = mean(x0)
    ret[, 1] = x0 + Z %*% rnorm(nc, 0, mn)/sqrt(nc)
    k = 0
    cat("Created Vectors: ")
    cat(paste(k))

    for (i in 2:(n + 1)) {
        ret[, i] = ret[, i - 1] + Z %*% rnorm(nc, 0, mn)/sqrt(nc)
        m = k + 1;
        while(any(ret[, i] < 0)) {
            reflection = rep(0, ncol(Emat))
            reflection[which(ret[, i] < 0)] = ret[, i][which(ret[, i] < 0)]
            for (j in 1:ncol(Z)) {
                ret[, i] = ret[, i] - 2* Z[, j] * (reflection %*% Z[, j])/sqrt(Z[,
                  j] %*% Z[, j])
            }
            ##for(i in 1:nchar(paste(k)))  cat("\b")
            ##cat(paste(m))
            ##k = m
            ##m = k + 1
        }
        for(i in 1:nchar(paste(k)))  cat("\b")
        cat(paste(m))
        k = m
    }
    ret = ret[, 2:(n + 1)]
    cat("\n")
    return(ret)
}
