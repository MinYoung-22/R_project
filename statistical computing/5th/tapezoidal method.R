Trapezodal = function(f, a, b,n, print= True){
  if( n <= 0){
    return("choose a positive integer n!")}
  x0 = sort(c(a,b))[1]; xn = sort(c(a,b))[2]
  h =(xn - x0)/n
  
  inegeral = 0 
  for (i in 1:n{ 
    integral = integral + 
      o.5 *h*(f(x0+i*h)+f(x0+(i-1)*h))
  }
  if(print == True){
  print(paste("Trapezoidal gives", integral))}
  return(integral = integral)
}