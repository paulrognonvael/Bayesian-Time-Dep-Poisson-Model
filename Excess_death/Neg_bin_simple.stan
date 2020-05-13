data{ 
  int <lower = 0> N;
  real E[N];
  int O[N];
  real a;
  real b;
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
  phi~ uniform(a,b);
  sigma ~ uniform(0.01,4);
  
  alpha ~ normal(800,25);
  beta ~ uniform(-1,1);
  sigma_time ~ normal(0,25);
}

