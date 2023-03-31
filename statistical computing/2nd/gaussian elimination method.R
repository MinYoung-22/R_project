A = matrix(c(4,2,8,6,-2,3,3,5,-6), ncol =3)
b = c(25, 13, -4)

##gasussian elimination method
solve(A,b)

##Back substitution with the upper triangular matrix
A = matrix(c(1,0, 0, 3/2, 1, 0, 3/4, 4/3, 1), ncol = 3)
b = c(25/4, 6, 3)
backsolve(A, b)