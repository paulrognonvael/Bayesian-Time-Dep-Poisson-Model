
sigma_a = 0.01
sigma_b = 1
alpha_mu = 0
alpha_sigma =0.5
beta_a = -1
beta_b = 1
sigma_time_mu = 0
sigma_time_sigma= 0.5



apriori_theta<- function(mean_data){
  sigma <- runif(1, sigma_a, sigma_b)
  #sigma_time <- runif(1, 0.01, 25)

  sigma_time <- rnorm(1, mean = sigma_time_mu, sd = sigma_time_sigma)
  while (sigma_time <= 0){
    sigma_time <- rnorm(1, mean = sigma_time_mu, sd = sigma_time_sigma)
  }
  
  alpha <- rnorm(1, mean = alpha_mu, sd = alpha_sigma)
  beta <- runif(1,beta_a,beta_b)
  log_theta <- rnorm(16, mean = 0, sigma)
  log_theta_time <- rep(0,16)
  log_theta_time[1] <-rnorm(1, mean = 0, sigma_time)
  for (i in 2:16){
    log_theta_time[1] <- rnorm(1, mean = alpha + beta*log_theta_time[i-1], sigma_time)
  } 
  theta_return <- exp(log_theta + log_theta_time)
  #O <- rep(0,16)
  #for (i in 1:16){
   # O[i] <- rpois(1, mean_england[i]*theta_return)
    #print(exp(log_theta[i]+ log_theta_time[i]))
    #print(mean_england[i]*exp(log_theta[i]+ log_theta_time[i]))
  #}
  return(theta_return)
  # return(theta_return)
}



england <- read.csv("/Users/aurorahofman/Documents/Utveksling/Baysiana/UPC-Final-Project-Bayesian-Analysis/Excess_death/Material_report_2/data/England_Wales_weekly.csv", header = T, sep = ';')
mean_england <- as.vector(as.matrix(england)[,12])
apriori_theta_england <- apriori_theta(mean_england)
matrix_theta <-c()
for (i in 1:10000){
  matrix_theta <- cbind(matrix_theta, apriori_theta(mean_england))
}

matrix_theta <- data.frame(matrix_theta)
mean_matrix <- rowMeans(matrix_theta)
median_matrix <- apply(matrix_theta,1, FUN = median)


matrix_theta_1 <- as.data.frame(matrix_theta[1:8,])
boxplot(t(matrix_theta_1), ylim = c(0, 10))
abline(h=1, col = "red")

apriori_O<- function(mean_data){
  sigma <- runif(1, sigma_a, sigma_b)
  #sigma_time <- runif(1, 0.01, 25)
  
  sigma_time <- rnorm(1, mean = sigma_time_mu, sd = sigma_time_sigma)
  while (sigma_time <= 0){
    sigma_time <- rnorm(1, mean = sigma_time_mu, sd = sigma_time_sigma)
  }
  
  alpha <- rnorm(1, mean = alpha_mu, sd = alpha_sigma)
  beta <- runif(1,beta_a,beta_b)
  log_theta <- rnorm(16, mean = 0, sigma)
  log_theta_time <- rep(0,16)
  log_theta_time[1] <-rnorm(1, mean = 0, sigma_time)
  for (i in 2:16){
    log_theta_time[1] <- rnorm(1, mean = alpha + beta*log_theta_time[i-1], sigma_time)
  } 
  theta_return <- exp(log_theta + log_theta_time)
  O <- rep(0,16)
  for (i in 1:16){
   O[i] <- rpois(1, mean_england[i]*theta_return)
  #print(exp(log_theta[i]+ log_theta_time[i]))
  #print(mean_england[i]*exp(log_theta[i]+ log_theta_time[i]))
  }
  return(O)
  # return(theta_return)
}


matrix_O <-c()

for (i in 1:10000){
  matrix_O <- cbind(matrix_O, apriori_O(mean_england))
}

matrix_O <- data.frame(matrix_O)
mean_matrix <- rowMeans(matrix_O)
median_matrix <- apply(matrix_O,1, FUN = median)

matrix_quantiles<- c()
for ( i in 1:16){
  quantiles<- quantile(as.vector(t(matrix_O[i,])),probs=c(0.05, 0.95), NA.rm=TRUE)
  matrix_quantiles<- cbind(matrix_quantiles, quantiles)
}


matrix_1 <- as.data.frame(matrix_O[1:8,])
boxplot(t(matrix_1), ylim = c(0, 100000))
abline(h=40000, col = "red")


apriori_gamma<- function(mean_data){
  sigma <- runif(1, sigma_a, sigma_b)
  #sigma_time <- runif(1, 0.01, 25)
  
  sigma_time <- rnorm(1, mean = sigma_time_mu, sd = sigma_time_sigma)
  while (sigma_time <= 0){
    sigma_time <- rnorm(1, mean = sigma_time_mu, sd = sigma_time_sigma)
  }
  
  alpha <- rnorm(1, mean = alpha_mu, sd = alpha_sigma)
  beta <- runif(1,beta_a,beta_b)
  theta <- rgamma(16, shape = 1.5, scale = 1/1.5)
  log_theta_time <- rep(0,16)
  log_theta_time[1] <-rnorm(1, mean = 0, sigma_time)
  for (i in 2:16){
    log_theta_time[1] <- rnorm(1, mean = alpha + beta*log_theta_time[i-1], sigma_time)
  } 
  theta_return <-theta + exp(log_theta_time)
  O <- rep(0,16)
  for (i in 1:16){
    O[i] <- rpois(1, mean_england[i]*theta_return)
    #print(exp(log_theta[i]+ log_theta_time[i]))
    #print(mean_england[i]*exp(log_theta[i]+ log_theta_time[i]))
  }
  return(theta_return)
  # return(theta_return)
}


apriori_gamma_O<- function(mean_data){
  sigma <- runif(1, sigma_a, sigma_b)
  #sigma_time <- runif(1, 0.01, 25)
  
  sigma_time <- rnorm(1, mean = sigma_time_mu, sd = sigma_time_sigma)
  while (sigma_time <= 0){
    sigma_time <- rnorm(1, mean = sigma_time_mu, sd = sigma_time_sigma)
  }
  
  alpha <- rnorm(1, mean = alpha_mu, sd = alpha_sigma)
  beta <- runif(1,beta_a,beta_b)
  theta <- rgamma(16, shape = 1.5, scale = 1/1.5)
  log_theta_time <- rep(0,16)
  log_theta_time[1] <-rnorm(1, mean = 0, sigma_time)
  for (i in 2:16){
    log_theta_time[1] <- rnorm(1, mean = alpha + beta*log_theta_time[i-1], sigma_time)
  } 
  theta_return <-theta + exp(log_theta_time)
  O <- rep(0,16)
  for (i in 1:16){
    O[i] <- rpois(1, mean_england[i]*theta_return)
    #print(exp(log_theta[i]+ log_theta_time[i]))
    #print(mean_england[i]*exp(log_theta[i]+ log_theta_time[i]))
  }
  return(O)
  # return(theta_return)
}
mean_england <- as.vector(as.matrix(england)[,12])
apriori_theta_england <- apriori_gamma(mean_england)
matrix_theta <-c()
for (i in 1:10000){
  matrix_theta <- cbind(matrix_theta, apriori_gamma(mean_england))
}

matrix_theta <- data.frame(matrix_theta)
mean_matrix <- rowMeans(matrix_theta)
median_matrix <- apply(matrix_theta,1, FUN = median)


matrix_theta_1 <- as.data.frame(matrix_theta[1:8,])
boxplot(t(matrix_theta_1), ylim = c(0, 10))
abline(h=1, col = "red")

matrix_O <-c()
for (i in 1:10000){
  matrix_O <- cbind(matrix_O, apriori_gamma_O(mean_england))
}

matrix_O <- data.frame(matrix_O)
mean_matrix <- rowMeans(matrix_O)
median_matrix <- apply(matrix_O,1, FUN = median)


matrix_O_1 <- as.data.frame(matrix_O[1:8,])
boxplot(t(matrix_O_1), ylim = c(0, 100000))
abline(h=40000, col = "red")

