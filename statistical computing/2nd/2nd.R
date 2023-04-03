#gaussian elimination medthod with R

A = matrix(c(4,2,8,6,-2,3,3,5,-6), ncol = 3)
b = c(25,13,-4)

solve(A,b)

#Back Subtitution
A = matrix(c(1,0,0,3/2,1,0,3/4,4/3,1), ncol = 3) 
b = c(25/4, 6,3)
backsolve(A,b)

#Gaussian elimination method + round-off error
A = matrix(c(0.00001,1,2,1), ncol=2)
b = c(2,2)
Ab = cbind(A,b)
solve(A,b)
#해결방법
options(digits=3)
A = matrix(c(0.00001, 1,2,1), ncol =2)
b = c(2,2)
Ab = cbind(A,b)
solve(A,b)
