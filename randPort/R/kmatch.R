
kmatch <- function(data, match.var, weight.var, sumlimit = NULL, ...) {
  A = matrix(0, nrow = length(match.var), ncol = nrow(data))
  b = A %*% data[[weight.var]]
  if(!is.null(sumlimit)) {
    A = rbind(A, sumlimit)
    b = c(b, sumlimit)
  }
  weights = hitandrun(A, b, ...)
  return(weights)
}