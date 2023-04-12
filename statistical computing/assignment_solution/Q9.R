Question9 <- function(A) {
  # Get dimensions of A (Note that A is an n by n matrix)
  n <- nrow(A)
  
  # Initialize Q, R and V matrices
  Q <- R <- V <- matrix(0, nrow=n, ncol=n)
  
  ## Perform Gram-Schmidt process
  # The first vector of the resulting orthonormal basis(v1) is equal to the first column of A
  V[,1] = A[,1]
  Q[,1] = A[,1] / sqrt(sum(A[,1]^2)) # sum(A[,1]^2) = c(A[,1] %*% A[,1])
  
  # vi (i>1)
  for (i in 2:n) {
    # For each subsequent column of A, subtract its projection onto the previous columns of V
    V[,i] = A[,i] 
    for (j in 1:(i-1)) {
      V[,i] = V[,i] - (sum(V[,j] * A[,i]) / sum(V[,j]^2)) * V[,j] 
    }
    # Normalizing
    Q[,i] = V[,i] / sqrt(sum(V[,i]^2))
  }
  
  # Compute R
  for (i in 1:n) {
    for (j in i:n) {
      R[i,j] = sum(Q[,i] * A[,j])
    }
  }
  return(list(Q = Q, R = R))
}

# test data
A <- matrix(c(2,5,1,3,5,1,9,-3,1), ncol = 3)

# run QR decomposition
result <- Question9(A)
print(result)