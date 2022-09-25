SODE=function(f, g, t0, y0, y0p, tf, n){
  eps= (tf- t0)/n
  y=y0
  x=y0p
  t=t0
  for (i in 1:n){ 
    x = x + f(t,x,y)*eps
    y = y + g(t,x,y)*eps
    t = t + eps
  }
  y
}
f=function(t,x,y){
  c(t) - a(t)*x - b(t)*y
}
g=function(t,x,y){x}

a=function(t){
  -t
}
b=function(t){
  1
}
c=function(t){
  log(t^2 + 1)
}

SODE(f, g, 0, 1, 0, 1, 1e3 )