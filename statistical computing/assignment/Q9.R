# Define a function for QR decomposition of a square matrix
QR_decomposition <- function(A) {
  n <- nrow(A)
  Q <- matrix(0, n, n)
  R <- matrix(0, n, n)
  
  # Compute the first column of Q
  Q[, 1] <- A[, 1] / norm(A[, 1])
  
  # Compute the first row of R
  R[1, 1] <- norm(A[, 1])
  
  # Compute the remaining columns of Q and rows of R using Gram-Schmidt algorithm
  for (j in 2:n) {
    v <- A[, j]
    for (i in 1:(j-1)) {
      R[i, j] <- t(Q[, i]) %*% A[, j]
      v <- v - R[i, j] * Q[, i]
    }
    R[j, j] <- norm(v)
    Q[, j] <- v / R[j, j]
  }
  
  # Return Q and R as a list
  return(list(Q = Q, R = R))
}
