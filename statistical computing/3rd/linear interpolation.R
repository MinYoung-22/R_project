Linear_Interpolation = function(f, x0, x1, eps = 10^(-5)){
  if (f(x0) * f(x1) >0){
    return("wrong initial interval!")}
  N = 0; err = 10; errors = NULL
  while(err > eps & N <= 1000){
    x2 = (f(x1)*x0 - f(x0)*x1) / (f(x1) - f(x0))
    err = abs(f(x2) - 0); errors= c(errors, err); N = N + 1
    if (f(x0) * f(x2) < 0){x1 = x2
    } else {
      x0 = x2}
  }
  return(list(solution = x2, errors = round(errors,4)))
}


install.packages("tictoc")
library("tictoc")

my_fun = function(x){
  x^2 + x - 5}

x0 = -5; x1 = 0 

tic()
Linear_Interpolation(my_fun,x0,x1,eps = 10^(-5))
toc()
