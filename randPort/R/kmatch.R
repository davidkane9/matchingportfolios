
kmatch <- function(data, match.var, weight.var, sumlimit = NULL, ...) {
  
  Alist = list()
  for(i in 1:length(match.var)) {
    if(class(data[[match.var[i]]]) == "numeric") {
      Alist[[i]] = data[[match.var[i]]]
    } else {
      Alist[[i]] = .dummy(data[[match.var[i]]])
    }
  }
  A = do.call(rbind, Alist)
  b = A %*% data[[weight.var]]
  
  if(!is.null(sumlimit)) {
    A = rbind(A, rep(1, ncol(A)))
    b = c(b, sumlimit)
  }
  weights = hitandrun(A, b, ...)
  return(weights)
}