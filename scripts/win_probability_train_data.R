# A script to generate the training data

# Author: Mac Bagwell

# Libraries
library(tidyverse)

# Parameters
ncaa_file <- here::here("data/ncaa.rds")
out_file <- here::here("data/win_probability_train_data.rds")
#===============================================================================

# Code
ncaa_file %>% 
  read_rds() %>% 
  transmute(
    seed_1 = pmin(team_1_seed, team_2_seed),
    seed_2 = pmax(team_1_seed, team_2_seed),
    y = !xor(team_1_seed < team_2_seed, team_1_score > team_2_score)
  ) %>% 
  write_rds(out_file)
