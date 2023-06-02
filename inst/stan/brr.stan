data {
  int<lower=0> N; // number of data items
  int<lower=0> K; // number of predictors
  matrix[N, K] x; // predictor matrix
  vector[N] y; // outcome vector
}
parameters {
  vector[K] beta; // coefficients for predictors
  real<lower=0> tau; // precision parameter
}
transformed parameters {
  real<lower=0> sigma; // standard deviation
  sigma = 1 / sqrt(tau); // Convert precision to standard deviation
}
model {
  tau ~ gamma(2, 0.1); // Gamma prior on the precision
  beta ~ normal(0, 100); // Normal prior on the coefficients
  y ~ normal(x * beta, sigma); // Likelihood
}

generated quantities {
 real y_rep[N];

 for (n in 1:N) {
 y_rep[n] = normal_rng(x[n] * beta, sigma);
 }

}
