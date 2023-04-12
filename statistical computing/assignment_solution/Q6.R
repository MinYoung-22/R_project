Question6 <- function(mtrx) {
  for (i in 1:2) {
    max_index <- which.max(abs(mtrx[i:3, i])) + (i - 1) # Find the entry in the i-th column with the largest absolute value. (This entry is pivot.)
    if (max_index != i) {
      mtrx[c(i, max_index), ] <- mtrx[c(max_index, i), ] # Perform a row exchange if necessary so that the pivot is in the i-th row.
    }
    for (j in (i+1):3) { # Perform Gaussian elimination
      mult <- mtrx[j, i] / mtrx[i, i]
      mtrx[j, ] <- mtrx[j, ] - mult * mtrx[i, ]
    }
  }
  return(mtrx)
}

# test data
A <- matrix(c(4,2,8,6,-2,3,3,5,-6), nrow = 3)
b <- matrix(c(25,13,-4), ncol = 1)
Ab <- cbind(A,b)

# run Gaussian elimination method with partial pivoting
result <- Question6(Ab)
print(result)