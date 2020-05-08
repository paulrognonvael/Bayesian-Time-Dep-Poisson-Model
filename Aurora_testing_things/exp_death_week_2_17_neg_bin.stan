data{ 
  int <lower=0> N;
  int y1[N];
  int y2[N];
  int y3[N];
  int y4[N];
  real a;
  real b;
  real mu;
  real sigma;
}

parameters{
  real <lower=0, upper = 1> p1;
  real <lower=0, upper = 1> p2;
  real <lower=0, upper = 1> p3;
  real <lower=0, upper = 1> p4;
  
  real <lower=0> r1;
  real <lower=0> r2;
  real <lower=0> r3;
  real <lower=0> r4;
}

model{
  y1 ~ neg_binomial_2((1-p1)*r1/(p1), r1);
  y2 ~ neg_binomial_2((1-p2)*r2/(p2), r2);
  y3 ~ neg_binomial_2((1-p3)*r3/(p3), r3);
  y4 ~ neg_binomial_2((1-p4)*r4/(p4), r4);
  
  p1 ~ beta(a,b);
  p2 ~ beta(a,b);
  p3 ~ beta(a,b);
  p4 ~ beta(a,b);
  
  r1 ~ normal(mu, sigma);
  r2 ~ normal(mu, sigma);
  r3 ~ normal(mu, sigma);
  r4 ~ normal(mu, sigma);
  
}