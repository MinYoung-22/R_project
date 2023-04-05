QR_decomposition <- function(A) {
  n <- nrow(A)
  Q <- matrix(0, n, n)
  R <- matrix(0, n, n)
  
 
  Q[, 1] <- A[, 1] / sqrt(sum(A[, 1]^2))
  
  
  R[1, 1] <- sqrt(sum(A[, 1]^2))
  
  
  for (j in 2:n) {
    v <- A[, j]
    for (i in 1:(j-1)) {
      R[i, j] <- t(Q[, i]) %*% A[, j]
      v <- v - R[i, j] * Q[, i]
    }
    R[j, j] <- sqrt(sum(v^2))
    Q[, j] <- v / R[j, j]
  }
  
  
  return(list(Q = Q, R = R))
}

A = matrix(c(2,5,1,3,5,1,9,-3,1), ncol =3)
A
QR_decomposition(A)
