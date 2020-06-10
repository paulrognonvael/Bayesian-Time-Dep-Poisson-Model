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
  real sigma_time_a;
  real sigma_time_b;
  
}

parameters{
  real log_theta[N];
  real log_theta_time[N];
  real <lower  = 0.01, upper=0.2> sigma;
  
  real alpha;
  real <lower=-1, upper=1> beta;
  real<lower=0.01, upper=0.2> sigma_time;
}

model{
  O[1] ~ poisson(E[1]* exp(log_theta[1]));
    log_theta[1]~ normal(0,sigma);
    log_theta_time[1] ~ normal(0,sigma);
  for (i in 2:N){
    O[i] ~ poisson(E[i]* exp(log_theta[i] + log_theta_time[i]));
    log_theta[i]~ normal(0,sigma);
    log_theta_time[i] ~ normal(alpha+beta*log_theta_time[i-1], sigma_time);
  }
  sigma ~ uniform(sigma_a,sigma_b);
  
  alpha ~ normal(alpha_mu,alpha_sigma);
  beta ~ uniform(beta_a,beta_b);
  sigma_time ~ uniform(sigma_time_a,sigma_time_b);
}

generated quantities{
  real sim[N];
  sim[1] = poisson_rng(E[1]* exp(log_theta[1]));
  for (i in 2:N){
    sim[i] = poisson_rng(E[i]* exp(log_theta[i] + log_theta_time[i]));
  }
}



