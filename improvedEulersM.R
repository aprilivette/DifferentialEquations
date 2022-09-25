improved.eulers.method= function(f, t0, tf, y0, n){
  eps=(tf-t0)/n
  y=y0
  t=rep(NA, (n+1))
  y=rep(NA,(n+1))
  z=rep(NA, n)
  y[1]=y0
  t[1]=t0
  for(i in 1:n){ 
    z[i]= y[i] + eps*f(t[i], y[i])
    y[i+1]= y[i] + (1/2)*eps*(f(t[i], z[i]) + f(t[i], y[i]))
    t[i+1]= t[i] + eps 
  }
  
  plot(y, t, type="l")  
  
}
f=function(t,y){
  t+cos(2*y)
}
improved.eulers.method(f, 1, 6, pi, 1e3)