data{ 
  int <lower = 0> N;
  real E[N];
  int O[N];
  real a;
  real b;
}

parameters{
  real theta[N];
  real <lower  = 0> phi;
  real <lower  = 0> sigma;
}

model{
  for (i in 1:N){
    O[i] ~ neg_binomial_2(E[i]* exp(theta[i]),phi);
    theta[i]~ normal(0,sigma);
  }
  phi~ uniform(a,b);
  sigma ~ uniform(0.01,4);
}

