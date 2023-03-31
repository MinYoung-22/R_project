# Define a matrix A
A <- matrix(c(2, 1, 1, 1, 3, 2, 1, 0, 0), nrow = 3, ncol = 3)

# Perform QR decomposition
Q <- qr.Q(qr(A))
R <- qr.R(qr(A)) #쓰지 말고 함수를 만들어 보세요


# Print the matrices Q and R
print(Q)
print(R)
