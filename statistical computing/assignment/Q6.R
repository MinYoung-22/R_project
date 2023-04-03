A = matrix(c(3,2,4,5,-2,4,-2,4,-1,17,6,19), ncol = 4)

n <- length(A)
x <- vector()
b <- A[, p]
for (i in n:1) {
  if (n >= (i + 1)) {
    x[i] <- (b[i] - sum(x[j] * A[i, j], j = (i + 1):n)) / A[i, i]
  } else {
    x[i] <- b[i] / A[i, i]
  }
}
x


