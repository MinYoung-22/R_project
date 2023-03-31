# Define an n by p matrix A
A <- matrix(c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12), nrow = 3)

# Define a function to perform Gaussian elimination with partial pivoting
gaussian_elimination_partial_pivot <- function(A) {
  n <- nrow(A)
  p <- ncol(A) - 1 # Exclude the last column which contains the constants
  
  # Forward elimination
  for (i in 1:(n-1)) {
    # Partial pivoting
    pivot <- i
    for (j in (i+1):p) {
      if (abs(A[pivot,i]) < abs(A[j,i])) {
        pivot <- j
      }
    }
    if (pivot != i) {
      A[c(i,pivot),] <- A[c(pivot,i),]
    }
    
    # Gaussian elimination
    for (j in (i+1):n) {
      multiplier <- A[j,i]/A[i,i]
      A[j,(i+1):p+1] <- A[j,(i+1):p+1] - multiplier * A[i,(i+1):p+1]
    }
  }
  
  # Backward substitution
  x <- numeric(p)
  for (i in p:1) {
    x[i] <- (A[i,p+1] - sum(A[i,(i+1):p] * x[(i+1):p]))/A[i,i]
  }
  
  # Return the solution vector x
  return(x)
}

# Call the function and print the solution vector x
gaussian_elimination_partial_pivot(A)

