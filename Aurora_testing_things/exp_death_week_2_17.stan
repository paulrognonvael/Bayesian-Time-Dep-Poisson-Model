data{ 
  int <lower=0> N;
  int y1[N];
  int y2[N];
  int y3[N];
  int y4[N];
  real a;
  real b;
}

parameters{
  real <lower=0> lambda1;
  real <lower=0> lambda2;
  real <lower=0> lambda3;
  real <lower=0> lambda4;
}

model{
  y1 ~ poisson(lambda1);
  y2 ~ poisson(lambda2);
  y3 ~ poisson(lambda3);
  y4 ~ poisson(lambda4);
  lambda1 ~ gamma(a,b);
  lambda2 ~ gamma(a,b);
  lambda3 ~ gamma(a,b);
  lambda4 ~ gamma(a,b);
}

