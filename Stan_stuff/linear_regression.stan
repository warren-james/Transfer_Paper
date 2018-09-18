data {
  int<lower = 0> N; // number of data points
  vector[N] y;  
  vector[N] x;
}
parameters {
  real c;
  real b; 
  real<lower = 0> sigma;
}
model {
  c ~ normal(0,10);
  b ~ normal(0,10);
  sigma ~ cauchy(0,5);
  y ~ normal(b * x + c, sigma);
}
