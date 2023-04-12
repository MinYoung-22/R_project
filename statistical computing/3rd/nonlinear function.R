x_value= seq(-4,4, by = 0.001) #등차수열
x_value

func_y = function(x){ 
  y = x^2 -5
}

y_value = func_y(x_value)

plot(x_value, y_value, type = "l",
     xlab = "x", ylab = "f(x)", xlim= c(-4,1.5)) 

abline(h = 0, col = "blue")
points(-sqrt(5), 0, pch= 19)


#appendix: R code for the three plots

x_value = seq(-4, 4, by = 0.001)

func_y1 = function(x){x^2-4}
func_y2 = function(x){x^2}
func_y3 = function(x){0.01/(x-2)}

y_value1 = func_y1(x_value)
y_value2 = func_y2(x_value)
y_value3 = func_y3(setdiff(x_value)) #setdiff()는 차집합 x_value에서 2만 제외


par(mfrow = c(2,2)) #한번에 여러 함수 그리기 

plot(x_value, y_value1, type = 'l',
     xlab = 'x', ylab = 'f(x)', xlim = c(-4,4))
abline(v = c(-3.5, 1), col= 'red', lty = 2)
points(c(-2,2), c(0,0), pch = 19)

plot(x_value, y_value2, type = "l",
     xlab = 'x', ylab = 'f(x)', xlim = c(-4,4))
abline(v = c(-2,2), col = 'red', lty = 2)
points(0,0, pch = 19)

plot(x_value, y_value2, type = "l", 
     xlab = "x", ylab = 'f(x)', xlim = c(-4,4))

abline(a = 1, b = -1, col = 'red', lty = 2)
abline(a= 2, b = 0, col = 'blue', lty = 2)
abline(h = 8, col = 'green', lty = 1)
abline(v =2, lty = 3)
