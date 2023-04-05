# A: n x n matrix
# b: n x 1 vector
# tol: tolerance for convergence
# max.iter: maximum number of iterations

gauss_seidel <- function(A, b, tol = 1e-10, max.iter = 1000) {
  n <- nrow(A)
  x <- numeric(n)  # initial guess of x is set to 0
  for (iter in 1:max.iter) {
    for (i in 1:n) {
      x_i <- b[i]
      for (j in 1:n) {
        if (j != i) {
          x_i <- x_i - A[i, j] * x[j]
        }
      }
      x[i] <- x_i / A[i, i]
    }
    if (max(abs(A %*% x - b)) < tol) {
      break
    }
  }
  return(x)
}

A <- matrix(c(3,2,-1,1,-5,1,-1,1), ncol = 3)
b <- (4,1,-8,5)
gauss_seidel(A,b)