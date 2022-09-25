#estimating the location of the max of a function f using newtons method
#necessary to first calculate finite difference formulas for the sequence 

f=function(x){
  ( 1/(x^2 + 2*x +3) ) + (1/(x^2 + 3*x +5 ))
}
f=function(x){ #1st derivative
  ( initialf(x+ eps) - initialf(x - eps) ) / 2*eps
}
g=function(x){ #second derivative
  ( initialf(x + eps) - 2*initialf(x) + initialf( x + eps) ) / eps^2
}

eps=0.2

newtons.method=function(f, g, x0, n){
  x=rep(NA, n)
  x[1]=x0 - (f(x0)/g(x0))
  for (i in 2:n){
    x[i]=x[i-1] - (f(x[i-1])/g(x[i-1]))
  }
  print(x[n])
  
}

#use the intial guess x0=1

newtons.method(f, g, 1, 10)