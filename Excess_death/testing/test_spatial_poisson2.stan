data{ 
  int <lower = 0> N;
  real E[N];
  int O[N];
}

parameters{
  real  logtheta[N];
  real <lower  = 0> sigma[N];
}

model{
  for (i in 1:N){
    O[i] ~ poisson(E[i]* exp(logtheta[i]));
    logtheta[i]~ normal(0,sigma[i]);
    sigma[i] ~gamma(10,0.01);
  }
}
