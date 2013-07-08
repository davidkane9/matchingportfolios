
getWeights.quad <- function(A, b, n, verbose = TRUE) {

    w = matrix(0, ncol = n, nrow = ncol(A))
    if(verbose) cat("Portfolios created: 0")
    for(i in 1:n) {
        sol = solve.QP(Dmat = diag(2, ncol(A)), dvec = rnorm(ncol(A)), Amat = t(rbind(A, diag(1, ncol(A)))), bvec = c(b, rep(0, ncol(A))), meq = nrow(A))
        w[,i] = sol$solution
        if(verbose) {
            for(j in 1:nchar(paste(i-1))) cat("\b")
            cat(paste(i))
        }
    }
    return(w)
}
