data {
  int<lower=1> n; // total number of data points
  int<lower=0> m; // number of predictors
  matrix[n, m] x;    // x values
  vector[n] y;    // y values
}

parameters {
  #real a;              // alpha
  vector<lower=0, upper=100000000>[m] b;   // beta - money
  real<lower=0> sigma; // stdev
}

model {
#  b ~ cauchy(0, 5);
  
  y ~ normal(x * b, sigma);
}