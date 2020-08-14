# A script to simulate reseeded tournaments from the win probability model

# Author: Mac Bagwell

# Libraries
library(tidyverse)

# Parameters
posterior_file <- here::here("models/win_probability_model_posterior.rds")
out_file <- here::here("data/reseeded_tournament_sample.rds")
n_trials <- 1e4L
#===============================================================================

# Code
here::here("scripts/simulation_resources.R") %>% 
  source()

params <- 
  posterior_file %>% 
  read_rds()

sample_tournament(n_trials, params, simulate_reseeded_tournament, simulate_game) %>% 
  write_rds(out_file)
