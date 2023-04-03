A <- matrix(c(4, 2, 8, 6, -2, 3, 3, 5, -6, 25, 13, -4), ncol = 4)

# 행을 서로 바꾸는 함수 
swap_rows <- function(df, n, k) {
  row_n <- df[n, ]
  row_k <- df[k, ]
  df[n, ] <- row_k
  df[k, ] <- row_n
  return(df)
}

gaussian_elimination<- function(A){
  
  n <- nrow(A)
  p <- ncol(A)
  
  # partial pivoting
  for (i in 1:(n-1)) { 
    max_index <- which.max(abs(A[, i]))
    if(max_index != i) {
      A <- swap_rows(A, max_index, i)
    }
    for (j in (i+1):n) {
      quo <- A[j, i] / A[i, i]
      A[j, ] <- A[j, ] - quo * A[i, ]
    }
  }
  #back substitution
  x <- rep(0, p)
  b <- A[,p]
  
  for (k in n:1) {
    x[k] <- b[k]
    if (k != n){
      for (l in (k+1):p){
        x[k] <- x[k] - A[k,l]*x[l]
      }
    }
    x[k] <- x[k]/A[k,k]
  }
  
  return(x)
}

gaussian_elimination(A)

