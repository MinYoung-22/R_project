#1

x_value = seq(-3, 3, by = 0.001)

func_y = function(x)x^2-3
y_value = func_y(x_value)

plot(x_value, y_value, type = "l",
xlab = "x", ylab = "f(x)", xlim = c(-4,1.5), 
cex.lab = 2)

points(-2, 1, pch = 19)
abline(a = -7, b = -4, col = 'blue' )
legend(-1, 6, legend = c("y= x^2-3", "y = -4x-7"),
col = c("black", 'blue'), lty = 1, cex =2)

#2 
x_value = seq(-3,3, by = 0.001)

plot(x_value, y_value, type="l", 
     xlab = "", ylab="",
     xlim = c(-1.5,5),ylim = c(-5,6))

points(c(1,2),c(-2,1), pch = 19)
abline(a = -4, b = 2, col = "blue")
abline(a = -5, b = 3, col = "red", lty = 2)

legend(-1.5, 6, legend = c("f(x)" ,"tangent line at x", 
                           "forward differecne"), 
       col = c("black", 'blue', 'red'), lty = c(1,1,2))

axis(side= 1, at = c(1,2), labels = c("x", "x+h"))

