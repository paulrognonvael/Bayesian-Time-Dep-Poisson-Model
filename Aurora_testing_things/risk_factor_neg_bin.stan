data{ 
  int <lower = 0> N;
  real E[N];
  int O[N];
}

parameters{
  real logtheta[N];
  real mu;
  real sigma;
  real r[N];
}

transformed parameters{
  for (i in 1:N){
    real p[i] = (E[i]*exp(logtheta[i]))/(E[i]*exp(logtheta[i]) + r);
  }
}

model{
  for (i in 1:N){
    O[i] ~ neg_binomial_2((1-p[i])*r/(p[i]), r);
    
    logtheta[i]~ normal(mu,sigma);
  }
  
  mu ~ normal(0, 1);
  sigma~normal(0,10);
  r ~ uniform(0,100);
}