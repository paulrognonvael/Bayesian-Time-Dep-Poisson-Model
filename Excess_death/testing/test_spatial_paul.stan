data{ 
  int <lower = 0> N;
  int <lower = 0> Ny;
  real E[N];
  int O[N,Ny];
}

parameters{
  real theta[N,Ny];
  real <lower  = 0> phi;
  real <lower  = 0> sigma;
}

model{
  for (i in 1:N){
    for(j in 1:Ny){
     O[i,j] ~ neg_binomial_2(E[i]* exp(theta[i,j]),phi);
     theta[i,j]~ normal(0,sigma);
    }
   
  }
  phi~ gamma(0.01,0.01);
  sigma ~ gamma(0.01,0.01);
}





