# Define a 3x4 matrix A
A <- matrix(c(1, 2, -1, 8, 2, -3, 1, 1, 3, -1, -2, 1), nrow = 3, ncol = 4)

# Define a function to perform Gaussian elimination with partial pivoting
gaussian_elimination_partial_pivot <- function(A) {
  n <- nrow(A)
  
  # Forward elimination
  for (i in 1:(n-1)) {
    # Partial pivoting
    pivot <- i
    for (j in (i+1):n) {
      if (abs(A[j,i]) > abs(A[pivot,i])) {
        pivot <- j
      }
    }
    if (pivot != i) {
      A[c(i,pivot),] <- A[c(pivot,i),]
    }
    
    # Gaussian elimination
    for (j in (i+1):n) {
      multiplier <- A[j,i]/A[i,i]
      A[j,(i+1):n+1] <- A[j,(i+1):n+1] - multiplier * A[i,(i+1):n+1]
    }
  }
  
  # Backward substitution
  x <- numeric(n)
  for (i in n:1) {
    x[i] <- (A[i,n+1] - sum(A[i,(i+1):n] * x[(i+1):n]))/A[i,i]
  }
  
  # Return the solution vector x
  return(x)
}

# Call the function and print the solution vector x
x <- gaussian_elimination_partial_pivot(A)
print(x)

