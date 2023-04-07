Forward = function(f, x, h, print = TRUE) {
  
  if (h <= 0){
    return ("choose positive h!")
  }
  else{
    derivative = (f(x+h)-f(x))/h
    if(print == TRUE){
    print (paste("FD gives the derivative value",
                 derivative,
                 "of the function f at the point", x))}
  return(derivative)
  }
}
  

my_fun = function(x){x^2 +x -5}
my_fun2 = function(x){exp(sin(x)+cos(x)^2)}

#exact한 미분함수
my_fun_prime = function(x){2*x+1}
my_fun2_prime = function(x){cos(x)-2*sin(x)*cos(x)}

h_set = seq(1, 10^(-5), by = -10^(-2))

#실제값과의 차이 
result1 = sapply(h_set, Forward, f = my_fun, 
                 x =2,print = FALSE) - my_fun_prime(x =2)
round(abs(result1), 3)

result2 = sapply(h_set, Forward, f = my_fun2, 
                 x =2,print = FALSE) - my_fun2_prime(x =2)
round(abs(result1), 3)
