data{ 
  int <lower = 0> N;
  real E[N];
  int O[N];
}

parameters{
  real  logtheta[N];
  real mu;
  real sigma;
}

model{
  for (i in 1:N){
    O[i] ~ poisson(E[i]* exp(logtheta[i]));
    
    logtheta[i]~ normal(mu,sigma);
  }
  
  mu ~ normal(0, 1);
  sigma ~normal(0, 10);
}