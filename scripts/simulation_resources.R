# Resources for tournament simulation

# Author: Mac Bagwell

# Libraries
library(tidyverse)

# Parameters
ROUND_NAMES <- str_c("round", 1:5, sep = "_")
#===============================================================================

# Code
simulate_game <- function(seeds, beta, delta) {
  x <- 1:15 %in% min(seeds):(max(seeds) - 1)
  z <- (beta * (x %*% delta))[1,1]
  prob <- 1 / (1 + exp(-z))
  
  if_else(runif(1) < prob, min(seeds), max(seeds))
}

force_upset <- function(winner, loser) {
  function(seeds, beta, delta) {
    if (winner %in% seeds && loser %in% seeds) {
      return(winner)
    }
    
    x <- 1:15 %in% min(seeds):(max(seeds) - 1)
    z <- (beta * (x %*% delta))[1,1]
    prob <- 1 / (1 + exp(-z))
    
    if_else(runif(1) < prob, min(seeds), max(seeds))
  }
}

simulate_tournament <- function(beta, delta, simulation_policy) {
  rounds <- vector(mode = "list", length = 5L)
  rounds[[1]] <- list(
    c(1L, 16L),
    c(8L, 9L),
    c(5L, 12L),
    c(4L, 13L),
    c(6L, 11L),
    c(3L, 14L),
    c(7L, 10L),
    c(2L, 15L)
  )
  
  for (r in 2:5) {
    winners <- map_int(rounds[[r - 1]], simulation_policy, beta, delta)
    
    if (r < 5) {
      rounds[[r]] <- 
        map(
          1:(length(winners) / 2), 
          ~ c(winners[(2 * . - 1):(2 * .)])
        )
    } else {
      rounds[[r]] <- winners
    }
  }
  
  rounds %>% 
    set_names(ROUND_NAMES)
}

simulate_reseeded_tournament <- function(beta, delta, simulation_policy) {
  rounds <- vector(mode = "list", length = 5L)
  
  rounds[[1]] <- list(
    c(1L, 16L),
    c(8L, 9L),
    c(5L, 12L),
    c(4L, 13L),
    c(6L, 11L),
    c(3L, 14L),
    c(7L, 10L),
    c(2L, 15L)
  )
  
  for (r in 2:5) {
    winners <- 
      map_int(rounds[[r - 1]], simulation_policy, beta, delta) %>% 
      sort()
    
    if (r < 5) {
      rounds[[r]] <- 
        map(
          1:(length(winners) / 2), 
          ~ c(winners[.], winners[length(winners) - . + 1])
        )
    } else {
      rounds[[r]] <- winners
    }
  }
  
  rounds %>% 
    set_names(ROUND_NAMES)
}

sample_tournament <- function(n_trials, params, tournament_policy, simulation_policy) {
  list(
    beta = params$beta,
    delta = array_branch(params$delta, margin = 1)
  ) %>% 
    shuffle(n = n_trials) %>% 
    pmap(tournament_policy, simulation_policy = simulation_policy)
}

shuffle <- function(l, n) {
  idx <- sample(1:length(l[[1]]), size = n, replace = TRUE)
  l %>% map(~ .[idx])
}
