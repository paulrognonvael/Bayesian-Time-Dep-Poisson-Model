data{ 
  int <lower = 0> N;
  real E[N];
  int O[N];
  real mu1;
  real mu2;
  real sigma1;
  real sigma2;
}

parameters{
  real  logtheta[N];
  real mu[N];
  real <lower  = 0> sigma[N];
}

model{
  for (i in 1:N){
    O[i] ~ poisson(E[i]* exp(logtheta[i]));
    logtheta[i]~ normal(mu[i],sigma[i]);
  }
  
  mu ~ normal(mu1, sigma1);
  sigma ~normal(mu2, sigma2);
}