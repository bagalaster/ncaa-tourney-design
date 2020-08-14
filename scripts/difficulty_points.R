# A script to compute the difficulty points

# Author: Mac Bagwell

# Libraries
library(tidyverse)

# Parameters
out_file <- here::here("data/difficulty_points.rds")
#===============================================================================

# Code
D <- matrix(0, nrow = 16, ncol = 6)
D[,1] <- c(1:16)
D[,2] <- c(9:16, 16:9)
D[,3] <- rep(c(13:16, 16:13), 2)
D[,4] <- rep(c(15, 16, 16, 15), 4)
D[,5] <- rep(16, 16)
D[,6] <- rep(16, 16)

D %>% 
  rowSums() %>% 
  enframe(name = "seed", value = "diff_pts") %>%
  write_rds(out_file)
