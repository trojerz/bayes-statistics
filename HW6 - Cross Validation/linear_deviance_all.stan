data {
  int<lower=1> n;       // total number of observation
  int<lower=0> m;       // number of independent variables
  matrix[n, m] X; // independent variables
  vector[n] y;          // observations
}

parameters {
  real b[m+11];          // betas
  real<lower=0> sigma; // stdev
}

model {
  for (i in 1:n) {
    // storage for linear terms
    real mu[m+10];
    
    // calculate terms
    for (j in 1:m) {
      mu[j] = X[i,j] * b[j+1];
    }
    mu[m+1] = X[i, 1] .* X[i, 2] * b[m+2];
    mu[m+2] = X[i, 1] .* X[i, 3] * b[m+3];
    mu[m+3] = X[i, 1] .* X[i, 4] * b[m+4];
    mu[m+4] = X[i, 1] .* X[i, 5] * b[m+5];
    mu[m+5] = X[i, 2] .* X[i, 3] * b[m+6];
    mu[m+6] = X[i, 2] .* X[i, 4] * b[m+7];
    mu[m+7] = X[i, 2] .* X[i, 5] * b[m+8];
    mu[m+8] = X[i, 3] .* X[i, 4] * b[m+9];
    mu[m+9] = X[i, 3] .* X[i, 5] * b[m+10];
    mu[m+10] = X[i, 4] .* X[i, 5] * b[m+11];
    
    // model
    y[i] ~ normal(b[1] + sum(mu), sigma);
  }
}

generated quantities {
  // log-likelihood
  vector[n] log_lik;
  
  for (i in 1:n) {
    // mu
    real mu[m+10];
     
    // calculate terms
    for (j in 1:m) {
      mu[j] = X[i,j] * b[j+1];
    }
    mu[m+1] = X[i, 1] .* X[i, 2] * b[m+2];
    mu[m+2] = X[i, 1] .* X[i, 3] * b[m+3];
    mu[m+3] = X[i, 1] .* X[i, 4] * b[m+4];
    mu[m+4] = X[i, 1] .* X[i, 5] * b[m+5];
    mu[m+5] = X[i, 2] .* X[i, 3] * b[m+6];
    mu[m+6] = X[i, 2] .* X[i, 4] * b[m+7];
    mu[m+7] = X[i, 2] .* X[i, 5] * b[m+8];
    mu[m+8] = X[i, 3] .* X[i, 4] * b[m+9];
    mu[m+9] = X[i, 3] .* X[i, 5] * b[m+10];
    mu[m+10] = X[i, 4] .* X[i, 5] * b[m+11];
    
    log_lik[i] = normal_lpdf(y[i] |(b[1] + sum(mu)), sigma);
  }
}