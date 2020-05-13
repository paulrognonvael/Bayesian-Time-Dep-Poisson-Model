# trying to fit an autoregressive bayesian model in order to assess time-dependency in the weekly data.
library(rstan)
data <- read.csv("data/England_Wales_weekly.csv", header = T, sep = ';')
data_2020 <- data[,13]
data_2019 <- data[,2]
N_2020 <- length(data_2020)
data_list_2020 <- list(N = N_2020,
                  y = data_2020)

data_list_2019 <- list (N = N_2020,
                        y = data_2019)



ar_england_2020 <- stan ("AR_1.stan", iter = 2000, chains = 4, data = data_list_2020, seed = 2707)
print(ar_england_2020)  

ar_england_2019 <- stan ("AR_1.stan", iter = 2000, chains = 4, data = data_list_2019, seed = 27071996)
print(ar_england_2019)
  
