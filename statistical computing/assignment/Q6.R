A <- matrix(c(2,2,3,5,-2,4,-2,4,-1,6,6,8), ncol= 4)
A


#partial pivoting
#swaping_1
max_index <-which.max(abs(A[,1]))
max_index

row_1 <- A[1,]


row_max <- A[max_index,]
A[max_index, ]<- row_1 
A[1,] <- row_max
A

#forward elimination_1
m1 <- A[2,1] /A[1,1]
A[2,]<-A[2,]-(A[1,]*m1)
m2 <- A[3,1]/A[1,1]
A[3,]<- A[3,]-(A[1,]*m2)


#swaping_2
max_index2 <-which.max(abs(A[2:3,2]))+1
max_index2
row_2 <- A[2,]
row_max2 <- A[max_index2,]
A[max_index2, ]<- row_2
A[2, ]<- row_max2
A

#forward elimination_2
m3 <- A[3,2]/A[2,2]
A[3,]<- A[3,]-(A[2,]*m3)
A

#back substitution
z<-A[3,4]/A[3,3]
y<-(A[2,4]- A[2,3]*z)/A[2,2]
x <-(A[1,4]- (A[1,3]*z+A[1,2]*y))/A[1,1]

result <- cbind(x,y,z)
result

