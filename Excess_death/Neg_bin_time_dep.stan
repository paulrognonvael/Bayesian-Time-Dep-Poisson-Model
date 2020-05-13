data{ 
  int <lower = 0> N;
  real E[N];
  int O[N];
  real phi_a;
  real phi_b;
  real sigma_a;
  real sigma_b;
  real alpha_mu;
  real alpha_sigma;
  real beta_a;
  real beta_b;
  real sigma_time_mu;
  real sigma_time_sigma;
  
}

parameters{
  real theta[N];
  real theta_time[N];
  real <lower  = 0> phi;
  real <lower  = 0> sigma;
  
  real alpha;
  real <lower=-1, upper=1> beta;
  real<lower=0> sigma_time;
}

model{
  O[1] ~ neg_binomial_2(E[1]* exp(theta[1]),phi);
    theta[1]~ normal(0,sigma);
    theta_time[1] ~ normal(0,sigma);
  for (i in 2:N){
    O[i] ~ neg_binomial_2(E[i]* exp(theta[i] + theta_time[i]),phi);
    theta[i]~ normal(0,sigma);
    theta_time[i] ~ normal(alpha+beta*theta_time[i-1], sigma_time);
  }
  phi~ uniform(phi_a,phi_b);
  sigma ~ uniform(sigma_a,sigma_b);
  
  alpha ~ normal(alpha_mu,alpha_sigma);
  beta ~ uniform(beta_a,beta_b);
  sigma_time ~ normal(sigma_time_mu,sigma_time_sigma);
}

