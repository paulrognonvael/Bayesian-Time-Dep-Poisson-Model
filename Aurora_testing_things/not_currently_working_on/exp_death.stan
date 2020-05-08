data{ 
  int <lower=0> N;
  int y[N];
  real a;
  real b;
}

parameters{
  real <lower=0> lambda;
}

model{
  y ~ poisson(lambda);
  
  lambda ~ gamma(a,b);
}

