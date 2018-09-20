data {
  int<lower = 0> N;                   // number of data points
  int<lower = 0, upper = 1> acc[N];   // Accuracy
  int<lower = 0, upper = 1> inst[N];  // Instructions
  int<lower = 0, upper = 1> half[N];  // Half
  int<lower = 0> S;                   // Number of Subjects
  int<lower = 0, upper = S> subj[S];  // Subject ID
}
parameters {
  real c;                      // Fixed intercept
  vector[S] subj_c;            // rand Intercepts by subj
  real b_i;                    // Main effect inst
  real b_h;                    // Main effect half
  real<lower = 0> sigma_subj;  // sd for subj implied uniform
}
model {
  real mu; 

  // priors
  c ~ normal(0,1);  
  subj_c ~ normal(0,sigma_subj);  // subj random effects?
  b_i ~ normal(0,1);
  b_h ~ normal(0,1);

  // likelihood
  for(n in 1:N) {
      mu = c + subj_c[subj[n]] + b_i * inst[n] + b_h * half[n];
      acc[n] ~ bernoulli_logit(fmax(0.5, mu));
  }
}