// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] y;
}

// The parameters accepted by the model. Our model
// accepts three parameters :
// alpha <- mean
// beta <- autocorrelation parameter in [-1,1]
// sigma <- std of the data
parameters {
  real alpha;
  real <lower=-1, upper=1> beta;
  real<lower=0> sigma;
}

// The model to be estimated. We model the output
// 'y_t' to be normally distributed with mean alpha+beta*y_(t-1)
// and standard deviation 'sigma'.
model {
  
  for (n in 2:N){
    y[n] ~ normal(alpha+beta*y[n-1], sigma);
  }
  
  // prior distributions
  alpha ~ normal(800,25);
  beta ~ uniform(-1,1);
  sigma ~ normal(0,25);

}

