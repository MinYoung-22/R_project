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
Forward(f = my_fun, x =2, h = -0.5)


Forward(f = my_fun, x = 2, h = 0.5)
Forward(f = my_fun, x = 2, h = 0.05)

h_set = seq(1, 10^(-5), by = -10^(-2))
sapply(h_set, Forward, f = my_fun, x =2, print = FALSE)

