data {
  int<lower = 0> N; // number of data points
  int<lower = 0, upper = 1> y[N];  
  vector[N] x;
}
parameters {
  real c;
  real b; 
}
model {
  c ~ normal(0,10);
  b ~ normal(0,10);
  y ~ bernoulli_logit(b * x + c);
}
