data{ 
  int <lower = 0> N;
  real E[N];
  int O[N];

  real sigma_a;
  real sigma_b;
  real alpha_mu;
  real alpha_sigma;
  real beta_a;
  real beta_b;
}

parameters{
  real log_theta[N];
  real <lower  = 0> sigma;
  
  real alpha;
  real <lower=-1, upper=1> beta;
}

model{
  O[1] ~ poisson(E[1]* exp(log_theta[1]));
    log_theta[1]~ normal(0,sigma);
  for (i in 2:N){
    O[i] ~ poisson(E[i]* exp(log_theta[i]));
    log_theta[i]~ normal(alpha+beta*log_theta[i-1],sigma);
  }
  sigma ~ uniform(sigma_a,sigma_b);
  
  alpha ~ normal(alpha_mu,alpha_sigma);
  beta ~ uniform(beta_a,beta_b);
}

