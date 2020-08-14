//
// A Stan Program that defines the seed-based win probability model 
//
data {
  int<lower=0> N; // # data points
  int<lower=0> D; // # delta params (# of seeds - 1)
  int seed_1[N]; 
  int seed_2[N];
  int y[N];
  
  real<lower=0> lambda; // beta prior param
  vector[D] alpha; // delta prior params
}

transformed data {
  // Indicator matrix for which delta params are active for each matchup
  matrix[N, D] X;
  for (i in 1:N) {
    row_vector[D] x_i = rep_row_vector(0, D);
    if (seed_1[i] < seed_2[i]) {
      for (j in seed_1[i]:(seed_2[i] - 1)) {
        x_i[j] = 1;
      }
    }
    
    X[i] = x_i;
  }
}

parameters {
  simplex[D] delta; // delta params
  real beta; // total effect size
}

model {
  y ~ bernoulli_logit(beta * (X * delta));
  delta ~ dirichlet(alpha);
  beta ~ exponential(lambda);
}

