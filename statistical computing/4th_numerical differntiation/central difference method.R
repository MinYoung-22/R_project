Central = function(f,x,h, print = TRUE){
  if(h <= 0){
    return("choose positive h!")}
  
  derivative = (f(x+h)-f(x-h))/(2*h)
  
  if (print == TRUE){
    print(paste("gives the derivative value",
                derivative, 
                "of the function f at the point", x))}
  return(derivative)
}

my_fun = function(x){x^5 +10*x^2 -10*x+3}

#exact한 미분 함수
my_fun_prime = function(x){5*x^4 + 20*x-10}

Central(f = my_fun, x = 1, h =0.05)

h_set = seq(1, 10^(-5), -10^(-2))

result = sapply(h_set, Central, 
                f= my_fun, x = 1, print=FALSE) - my_fun_prime(x =1)
result
