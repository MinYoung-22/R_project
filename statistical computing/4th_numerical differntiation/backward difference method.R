Backward = function(f,x,h, print = TRUE){
  if (h <= 0){
    return("choose positive h!")}
  
    derivative = (f(x) -f(x-h))/h
    
  if (print ==TRUE){ 
  print(paste("gives the derivative value",
              derivative,
              "of the function f at the point", x))}
  return(derivative)
}

my_fun = function(x) x^3 +2*x -5
Backward(f = my_fun, x =2, h=0.05)

#실제 미분 함수

my_fun_prime = function (x) {3*x^2 + 2}

h_set =seq(1, 10^(-5), by = -10^(-2))
result1 = sapply(h_set, Backward, f = my_fun, x =2,
                 print= FALSE) - my_fun_prime(x=2)

round(abs(result1), digits= 1) # round는 반올림하는 함수 
# sapply 주어진 각 행렬의 원소에 주어진 함수 적용