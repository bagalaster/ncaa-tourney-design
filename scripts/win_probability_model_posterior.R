# A script to train the win probability model

# Author: Mac Bagwell

# Libraries
library(tidyverse)

# Parameters
model_file <- here::here("models/win_probability_model.stan")
posterior_file <- here::here("models/win_probability_model_posterior.rds")
train_data_file <- here::here("data/win_probability_train_data.rds")

alpha <- c(3, 2, rep(1, 13L))
lambda <- 2
D <- 15L
#===============================================================================

# Code
train_data <- 
  train_data_file %>% 
  read_rds()

stan_data <- 
  list(
    N = nrow(train_data),
    D = D,
    seed_1 = train_data$seed_1,
    seed_2 = train_data$seed_2,
    y = train_data$y,
    lambda = lambda,
    alpha = alpha
  )

model_file %>% 
  rstan::stan(data = stan_data) %>% 
  rstan::extract(pars = c("beta", "delta")) %>% 
  write_rds(posterior_file)
