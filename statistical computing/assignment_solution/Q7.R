Question7 <- function(mtrx) {
  n <- nrow(mtrx)
  for (i in 1:(n-1)) {
    max_index <- which.max(abs(mtrx[i:n, i])) + (i - 1)
    if (max_index != i) {
      mtrx[c(i, max_index), ] <- mtrx[c(max_index, i), ]
    }
    for (j in (i+1):n) {
      mult <- mtrx[j, i]/mtrx[i, i]
      mtrx[j, ] <- mtrx[j, ] - mult * mtrx[i, ]
    }
  }
  return(mtrx)
}

# test data
A <- matrix(c(4,2,8,6,-2,3,11,5,-6,7,-3,5,-9,5,12,4,2,-5,9,7,-11,6,3,2), nrow = 4)
b <- matrix(c(25,13,-4,5), ncol = 1)
Ab <- cbind(A,b)

# run Gaussian elimination method with partial pivoting
result <- Question7(Ab)
print(result)