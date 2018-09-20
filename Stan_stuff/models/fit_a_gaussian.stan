data {
  int<lower = 0> N; // number of data points
  real y[N];  
  // real x[N];
}
parameters {
  real mu;
  real<lower = 0> sigma;
}
model {
  y ~ normal(mu,sigma);
}
