Bisection = function(f, x0, x1, eps = 10^(-5)){ 
  if (f(x0) * f(x1) >0){ 
    return("wrong initial interval!")}
  N = 0; err = x1 - x0 
  while(err > eps & N <= 1000){ x2 = (x0 + x1)/2
  if (f(x0) * f(x2) < 0){x1 = x2
  } else {
    x0 = x2}
  err = x1 - x0
  N = N + 1
  }
  return((x0 + x1)/2)
}

library('tictoc')

my_fun = function(x){
  x^2 + x - 5}

x0 = -5; x1 = 0 

Bisection(my_fun,x0,x1,eps = 10^(-5))

#continuous에만 가능 
#(x-a)^2 안됨