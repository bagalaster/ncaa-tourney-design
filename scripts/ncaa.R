# A script to download NCAA results
#
# Fetches data found here: https://data.world/michaelaroy/ncaa-tournament-results

# Author: Mac Bagwell

# Libraries
library(tidyverse)

# Parameters
download_link <- "https://query.data.world/s/pnnxob5aw2g2blg5woc6vw72djva2s"
output_file <- here::here("data/ncaa.rds")
raw_output_file <- here::here("data-raw/ncaa.csv")

col_names_ <- c(
  "year",
  "round",
  "region_id",
  "region_name",
  "team_1_seed",
  "team_1_score",
  "team_1_name",
  "team_2_name",
  "team_2_score",
  "team_2_seed"
)

col_types_ <- cols(
  region_name = col_character(),
  team_1_name = col_character(),
  team_2_name = col_character(),
  .default = col_integer()
)

rows_to_skip <- 1L

#===============================================================================

# Backup copy in original csv format
download_link %>% 
  download.file(raw_output_file)

# RDS format
download_link %>% 
  read_csv(
    col_names = col_names_,
    col_types = col_types_,
    skip = rows_to_skip
  ) %>% 
  write_rds(output_file)

  
