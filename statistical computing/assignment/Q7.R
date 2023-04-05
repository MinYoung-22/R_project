A <- matrix(c(2,2,3,5,-2,4,-2,4,-1,6,6,8), ncol= 4)
A
n <- nrow(A)
p <- ncol(A)


# 행을 서로 바꾸는 함수
swap_rows <-  function(A,x,y){
  row_x <-A[x,]
  row_y <-A[y,]
  A[y,]<- row_x
  A[x, ] <- row_y
  return (A)
}

# partial pivoting
for (i in 1:(n - 1)) {
  max_index <- which.max(abs(A[i:n, i])) + i - 1
  if (max_index != i) {
    A <- swap_rows(A, i, max_index)
  }
  for (j in (i + 1):n) {
    quo <- A[j, i] / A[i, i]
    A[j, ] <- A[j, ] - quo * A[i, ]
  }
}
A
# back substitution
x<- vector()
b<- A[ ,p]
b

for(i in n:1){
  for(j in n:(i+1)){
    if(n >=(i+1)){
      x[i] <- (b[i]-sum(x[j]*A[i,j]))/A[i,i]
    }else{
      x[i] <- b[i]/A[i,i]
    }
  }
}
x