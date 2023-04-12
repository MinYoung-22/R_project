#singular value decomposition

A = matrix(c(2,5,1,3,5,1,9,-3,1),ncol =3)
A = (A=t(A))
svd(A)

eigen(A)

svdA = svd(A)
# checking A = UDV
svdA$u %*% diag(svdA$d) %*% t(svdA$v)



                