data {
  int<lower = 0> N;                   // number of data points
  int<lower = 0, upper = 1> acc[N];   // Accuracy
  Vector[N] delta;                    // Separation
  int<lower = 0, upper = 1> inst[N];  // Instructions
  int<lower = 0, upper = 1> half[N];  // Half
  int<lower = 0> S;                   // Number of Subjects
  int<lower = 0, upper = S> subj[N];  // Subject ID
}

parameters {
  real c;                      // Fixed intercept
  vector[S] subj_c;            // rand Intercepts by subj
  real b_d;                    // Main effect delta
  real b_i;                    // Main effect inst
  real b_h;                    // Main effect half
  real b_hi;                   // interaction half inst
  real b_hd;                   // interaction half delta
  real b_id;                   // interaction inst delta
  real b_hid;                  // interaction half inst delta
  real<lower = 0> sigma_subj;  // sd for subj implied uniform
}

model {
  real mu; 

  // priors
  c ~ normal(0,1);  
  subj_c ~ normal(0,sigma_subj);  // random intercepts implied uniform
  b_d ~ normal(0,1);              // delta slope
  b_i ~ normal(0,1);              // inst main effect
  b_h ~ normal(0,1);              // half main effect
  b_hi ~ normal(0,1);             // half * inst
  b_hd ~ normal(0,1);             // half * delta
  b_id ~ normal(0,1);             // inst * delta
  b_hid ~ normal(0,1);            // threeway

  // likelihood
  for(n in 1:N) {
      mu = c + subj_c[subj[n]] + b_d * delta[n] + b_i * inst[n]
             + b_h * half[n]
             + b_hi * half[n] * inst[n]
             + b_hd * half[n] * delta[n]
             + b_id * inst[n] * delta[n]
             + b_hid * half[n] * inst[n] * delta[n];
      acc[n] ~ bernoulli_logit(fmax(0.5, mu));
  }
}