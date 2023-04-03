A <- matrix(c(3,2,-1,1,-5,1,-1,1,4,1,-8,5), ncol = 4)

n <- nrow(A)
p <- ncol(A) 

x <- vector()
b <- A[,p]

for (i in 1:n){
  x [i] <- 0 
}

for (k in 1:n){
  for (j in 1:(p-1)){
    x[k] <- (b[k]-(sum(x[j]*A[k,j]-x[k]*A[k,k])))/A[k,k]
  }
}
x


