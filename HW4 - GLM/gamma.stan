data {
  int<lower=1> n; // total number of data points
  int<lower=1> m; // number of predictors
  matrix[n, m] x;    // x values
  vector[n] y;    // y values
}

parameters {
  vector[m] b; // betas - regression parameter
  real phi; // variance parameter
  
}

transformed parameters {
  vector[n] ni;
  vector[n] lambda;
  vector[n] mu;
  
  mu = exp(x*b); //using the log link 
  
  ni = mu .* mu / phi;
  lambda = mu / phi;

}


model {
  b[1] ~ cauchy(0, 10);
  b[2:] ~ cauchy(0, 2.5);
  
  ni ~ exponential(1);
  
  y ~ gamma(ni, lambda);

}
