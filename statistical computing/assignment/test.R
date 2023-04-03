

# 행을 서로 바꾸는 함수 
swap_rows <- function(df, a, b) {
  row_a <- df[a, ]
  row_b <- df[b, ]
  df[a, ] <- row_b
  df[b, ] <- row_a
}

A<- matrix(c(4,2,8,6,-2,3,3,5,-6, 25,13,-4), nrow = 3)

gaussian_elimination <- function(A){
  
  n<- nrow(A)
  p<- ncol(A)

# partial pivoting
for(i in 1:(n-1)){
  max_index <-which.max(abs(A[,i]))
  if (max_index != i){ #필요하다면 서로 바꾸기 
    swap_rows(A, i, max_index)
  }
  
  #elimination
  for (j in (i+1): n){
    m <- A[j, i]/A[i:i]
    A[j,(i+1):p ] <- A[j,(i+1): p] - m *A[j, (i+1):p]
    
  }
}
  #bacak substitution
}


test_1<- matrix(c(4,2,8,6,-2,3,3,5,-6, 25,13,-4), nrow = 3)

gaussian_elimination(test_1)
