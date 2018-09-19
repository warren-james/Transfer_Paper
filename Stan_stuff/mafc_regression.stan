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
  for(n in 1:N)
      y[n] ~ bernoulli_logit(2 * fmax(0.5,
                             b * x[n] + c) - 1);
}
