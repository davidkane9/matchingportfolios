
find_x0 <- function(A, b) {

    x0 = t(A) %*% solve(A %*% t(A)) %*% b
    if(any(x0 < 0)) {
        ## project direction of greatest ascent onto k-plane
        u = rep(1, length(x0))

        ## do this via getting rid of the normal vectors
        for(i in 1:nrow(A)) {
            row = A[i,]
            projection = (u %*% row)/(row %*% row) * row
            u = u - projection
        }
    }
    return(u)

}
