data {
  int<lower = 0> N; // number of data points
  int<lower = 0, upper = 1> acc[N];  
  int<lower = 0, upper = 1> inst[N];  
  int<lower = 0, upper = 1> half[N];
  vector[N] delta;
}
parameters {
  real c;
  real b;
  real b_i; 
  real b_h;
}
model {
  c ~ normal(0,10);
  b ~ normal(0,10);
  for(n in 1:N)
      acc[n] ~ bernoulli_logit(2 * fmax(0.5,b * delta[n] + b_i * inst[n] + b_h * half[n] + c));
}
