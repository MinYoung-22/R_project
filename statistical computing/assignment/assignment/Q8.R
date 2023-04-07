gauss_seidel <- function(A, b, max.iter = 100, tol = 1e-6) {
  n <- length(b)
  x <- rep(0, n) 
  
  for (k in 1:max.iter) {
    for (i in 1:(n-1)) {
      x[i] <- (b[i] - A[i, 1:(i-1)] %*% x[1:(i-1)] - A[i, (i+1):n] %*% x[(i+1):n]) / A[i, i]
    }
    if (max(abs(A %*% x - b)) < tol) { # convergence
      break
    }
  }
  return(x)
}

A <- matrix(c(4, -1, 0, -1, 4, -1, 0, -1, 4), nrow = 3, ncol = 3)
b <- c(1, 2, 3)
gauss_seidel(A, b)
