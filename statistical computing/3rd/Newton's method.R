Newton_Method = function(f, fp, x, eps = 10^(-5)){
  # fp is the derivative function of f.
  N = 0; err = 10; err_set = NULL
  while(err > eps & N <= 1000){
    x0 = x
    x = x - f(x) / fp(x)
    err = abs(f(x))
    err_set = c(err_set, err)
    N = N + 1
  }
  return(list(solution = x, num_repeat = N,
              errors = round(err_set,4)))
}