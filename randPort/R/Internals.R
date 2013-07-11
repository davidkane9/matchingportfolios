## Does not work because correct direction is hard to find in this space.
## finds a positive solution, given some constraints
.beamSearch <- function(A, b, numjump = 100, verbose = FALSE) {
  ## resolve strange quirk with Null function
  if(ncol(A) ==1) {
    Z = Null(A)
  } else {
    Z = Null(t(A))
  }
  ## finds the point closest to zero  
  l = t(A) %*% solve(A %*% t(A)) %*% b
  
  mindist = sqrt(sum(l[which(l<0)]^2))
  mixnum = 0
  num = 0
  while(mindist > 0 && mixnum <= 5) {
    str = paste("Min dist: ", mindist, "\nNum: ", num, sep = "")
    if(verbose) cat(str)
    jumps = Z %*% matrix(rnorm(ncol(Z)*numjump, sd = mindist/sqrt(ncol(Z))), nrow = ncol(Z), ncol = numjump)
    dists = apply(jumps, 2, function(x) {
      np = l + x
      if(any(np < 0)) {
      dist = sqrt(sum(np[which(np < 0)]^2))
      } else {
        dist = -median(np)
      } 
    })
    if(mindist > min(dists)) {
      mindist = min(dists)
      index = which(dists == mindist)[1]
      l = l + jumps[,index]
    }
    if(all(l>0)) {
      mixnum = mixnum + 1
    }
    if(verbose) for(i in 1:nchar(str)) cat("\b")
    num = num + 1
  }
  return(l)
}