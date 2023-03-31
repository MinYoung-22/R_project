A = matrix(c(4,2,8, 6,-2,3,3,5,-6,25,13,-4), ncol = 4)
#행을 서로 바꾸는 함수 정의 
swap_rows <- function(df, n, k) {
  row_n <- df[n, ]
  row_k <- df[k, ]
  df[n, ] <- row_k
  df[k, ] <- row_n
  return(df)
}

# 1열에서 절대값이 가장 큰 행을 맨 위로 올리자 
max_first_col = which.max(abs(A[,1]))
max_first_col #몇 번째 행인지가 출력된다. 

A<- swap_rows(A,max_first_col,1)

sec_col_row_quo <- A[1,1]/ A[2,1]
third_col_row_quo <- A[1,1]/ A[3,1]


A[2, ] <- A[2,] -(A[1,]/sec_col_row_quo)
A[3, ]<- A[3,] -(A[1,]/third_col_row_quo)


# 2열에서 2번째 열 밑으로 절대값이 가장 큰 행을 두 번째로 올리자 
max_sec_col = which.max(abs(A[2:3,2])) #몇 번째 행인지가 출력된다. 

A<-swap_rows(A,max_sec_col,2)

third_col_row_quo2 <-A[2,2]/A[3,2]
A[3, ] <- A[3, ] - (A[2, ]/ third_col_row_quo2)
A
