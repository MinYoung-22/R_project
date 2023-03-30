# Define a matrix A and vector b
A <- matrix(c(4, -1, 0, 0, 3, -1, 0, 0, 3), nrow = 3, ncol = 3)
b <- c(3, 4, 4)

# Define the Gauss-Seidel function
gauss_seidel <- function(A, b, x0, tol, maxiter) {
  n <- length(b)
  x <- x0
  k <- 1
  converged <- FALSE
  
  while (!converged && k <= maxiter) {
    for (i in 1:n) {
      x[i] <- (b[i] - A[i,1:(i-1)] %*% x[1:(i-1)] - A[i,(i+1):n] %*% x[(i+1):n])/A[i,i]
    }
    
    # Check for convergence
    if (max(abs(A %*% x - b)) < tol) {
      converged <- TRUE
    }
    
    k <- k + 1
  }
  
  if (k > maxiter) {
    warning("Gauss-Seidel method did not converge within maximum number of iterations.")
  }
  
  # Return the solution vector x
  return(x)
}

# Set the initial guess, tolerance, and maximum number of iterations
x0 <- c(0, 0, 0)
tol <- 1e-6
maxiter <- 1000

# Call the Gauss-Seidel function and print the solution vector x
x <- gauss_seidel(A, b, x0, tol, maxiter)
print(x)
