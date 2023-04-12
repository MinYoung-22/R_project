Question8 <- function(A, b, x0, max_iter=1000, tolerance=10^(-6)) {
  n <- length(b)
  x <- x0
  iter <- 0
  error <- tolerance + 1
  
  while (error > tolerance) {
    for (i in 1:n) {
      x[i] <- (b[i] - sum(A[i, -i] * x[-i])) / A[i, i]
    }
    error <- max(abs(x-x0))
    x0 <- x
    iter <- iter + 1
    if (is.infinite(error)) {
      return(cat("The algorithm cannot converge."))
    }
    if (iter == max_iter) {
      return(cat("!! max iter"))
    }
  }
  return(x)
}

# test data
A <- matrix(c(12,3,-5,1,5,3,3,7,13), byrow = T, ncol = 3)
b <- matrix(c(1,28,76), ncol = 1)
x0 <- c(0,0,0)

# run Gauss-Seidel method
result <- Question8(A, b, x0)
print(result)