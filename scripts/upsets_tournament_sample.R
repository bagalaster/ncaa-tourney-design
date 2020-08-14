# A script to simulate tournaments from the win probability model conditioned on
# early round upsets

# Author: Mac Bagwell

# Libraries
library(tidyverse)

# Parameters
posterior_file <- here::here("models/win_probability_model_posterior.rds")
out_file <- here::here("data/upsets_tournament_sample.rds")
n_trials <- 1e4L
upsets <- list(c(16L, 1L), c(15L, 2L), c(14L, 3L), c(13L, 4L))
#===============================================================================

# Code
here::here("scripts/simulation_resources.R") %>% 
  source()

params <- 
  posterior_file %>% 
  read_rds()

upsets %>% 
  map(
    ~ sample_tournament(
      n_trials, 
      params, 
      simulate_tournament, 
      force_upset(.[1], .[2])
    )
  ) %>% 
  write_rds(out_file)

