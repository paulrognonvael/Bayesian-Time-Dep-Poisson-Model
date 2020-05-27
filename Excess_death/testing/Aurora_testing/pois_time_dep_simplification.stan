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
  real gamma_a;
  real gamma_b;
  
}

parameters{
  real log_theta[N];
  real <lower  = 0> sigma;
  
  real alpha;
  real <lower=-1, upper=1> beta;
  real <lower=0, upper= 10>gamma[N];
}

model{
  O[1] ~ poisson(E[1]* exp(log_theta[1]));
    log_theta[1]~ normal(gamma[1],sigma);
    gamma[1]~gamma(gamma_a, gamma_b);
  for (i in 2:N){
    O[i] ~ poisson(E[i]* exp(log_theta[i]));
    log_theta[i]~ normal(gamma[i]+alpha+beta*log_theta[i-1],sigma);
    gamma[i]~gamma(gamma_a, gamma_b);
  }
  sigma ~ uniform(sigma_a,sigma_b);
  
  alpha ~ normal(alpha_mu,alpha_sigma);
  beta ~ uniform(beta_a,beta_b);
}

