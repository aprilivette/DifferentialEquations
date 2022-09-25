#using eulers method to calculate the SIR model

SIR=function(beta, gamma, S0, I0, R0, tf, n ){
  eps= tf/n
  t=0
  S=rep(NA,n)
  I=rep(NA,n)
  R=rep(NA,n)
  S[1]=S0
  I[1]=I0
  R[1]=R0
  for(i in 2:n){
    S[i]= S[i-1]- (beta*S[i-1]*I[i-1]*t)
    I[i]= I[i-1] + (beta*S[i-1]*I[i-1] - gamma*I[i-1])*t
    R[i]= R[i-1]+ (gamma*I[i-1]*t)
    t=t+eps
  }
  plot(S, type="l")
  plot(I, type="l")
  plot(R, type="l")
  print(max(I))
}

SIR(2, 1, .999, .001, 0, 1, 1e5)

#this specific model spikes at 15.4% even when n is large 





