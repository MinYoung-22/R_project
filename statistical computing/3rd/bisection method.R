Bisection = function(f, x0, x1, eps = 10^(-5)){ #eps는 언제 멈추느냐를 결정( tolerance parameter)
  if (f(x0) * f(x1) >0){ # 두값의 부호가 같으면 root가 범위 내 없다 
    return("wrong initial interval!")}
  N = 0; err = x1 - x0 # N은 interval을 몇 번 나눌건지, err는 인터벌의 간격
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

tic()
Bisection(my_fun,x0,x1,eps = 10^(-5))
toc()

#continuous에만 가능 
#(x-a)^2 안됨