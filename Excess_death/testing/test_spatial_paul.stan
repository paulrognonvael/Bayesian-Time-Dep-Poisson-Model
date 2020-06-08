data{ 
  int <lower = 0> N;
  int <lower = 0> Ny;
  real E[N];
  int O[N,Ny];
}

parameters{
  real theta[N,Ny];
  real <lower  = 0> phi[N,Ny];
  real <lower  = 0> sigma[N,Ny];
  real <lower  = 0> mu[N,Ny];
}

model{
  for (i in 1:N){
    for(j in 1:Ny){
     O[i,j] ~ neg_binomial_2(E[i]* exp(theta[i,j]),E[i]* exp(theta[i,j])*phi[i,j]);
     theta[i,j]~ normal(mu[i,j],sigma[i,j]);
     mu[i,j] ~ normal(0,100);
     sigma[i,j] ~ uniform(0,100);
     phi[i,j] ~ uniform(0,100);
    }
  }
}





