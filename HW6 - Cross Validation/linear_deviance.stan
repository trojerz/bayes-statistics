data {
  int<lower=1> n;       // total number of observation
  int<lower=0> m;       // number of independent variables
  matrix[n, m] X; // independent variables
  vector[n] y;          // observations
}

parameters {
  real b[m+1];          // betas
  real<lower=0> sigma; // stdev
  real<lower=0> sigma_mu;
  real mu_mu;
}

model {
  for (i in 1:n) {
    // storage for linear terms
    real mu[m];
    
    // calculate terms
    for (j in 1:m) {
      mu[j] = X[i,j] * b[j+1];
    }
    // model
    y[i] ~ normal(b[1] + sum(mu), sigma);
  }
}

generated quantities {
  // log-likelihood
  vector[n] log_lik;
  
  for (i in 1:n) {
    // mu
    real mu[m];
     
    // calculate terms
    for (j in 1:m) {
      mu[j] = X[i,j] * b[j+1];
    }
    
    log_lik[i] = normal_lpdf(y[i] |(b[1] + sum(mu)), sigma);
  }
}