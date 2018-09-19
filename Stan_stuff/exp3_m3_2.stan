data {
  int<lower = 0> N; // number of data points
  int<lower = 0, upper = 1> acc[N];  
  int<lower = 0, upper = 1> inst[N];  
  int<lower = 0, upper = 1> half[N];
}
parameters {
  real c;
  real b_i; 
  real b_h;
  real b_hi;
}
model {
  real mu;

  // priors
  c ~ normal(0,1);
  b_i ~ normal(0,1);
  b_h ~ normal(0,1);
  b_hi ~ normal(0,1);

  // likelihood
  for(n in 1:N) {
      mu = b_i * inst[n] + b_h * half[n]
         + b_hi * inst[n] * half[n] + c;
      acc[n] ~ bernoulli_logit(fmax(0.5, mu));
  }
}
