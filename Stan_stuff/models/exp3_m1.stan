data {
  int<lower = 0> N; // number of data points
  int<lower = 0, upper = 1> acc[N];  
  vector[N] delta;
}
parameters {
  real c;
  real b; 
}
model {
  c ~ normal(0,10);
  b ~ normal(0,10);
  for(n in 1:N)
      acc[n] ~ bernoulli_logit(fmax(0,b * delta[n] + c));
}
