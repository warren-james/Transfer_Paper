data {
  int<lower = 0> N;                   // number of data points
  int<lower = 0, upper = 1> acc[N];   // Accuracy
  int<lower = 0, upper = 1> inst[N];  // Instructions
  int<lower = 0, upper = 1> half[N];  // Half
  vector[N] delta;                    // Number of trials
  int<lower = 0> S;                   // Number of Subjects
  int<lower = 0, upper = S> subj[S];  // Subject ID
}
parameters {
  real c;                      // Fixed intercept
  vector[S] subj_c;            // rand Intercepts by subj
  real b;                      // Slope
  real b_i;                    // Main effect inst
  real b_h;                    // Main effect half
  real<inter = 0> sigma_subj;  // sd for subj implied uniform
}
model {
  c ~ normal(0,10);  
  subj_c ~ normal(0,sigma_subj);  // subj random effects?
  b ~ normal(0,10);
  for(n in 1:N)
      acc[n] ~ bernoulli_logit(fmax(0,b * delta[n] + b_i * inst[n] + b_h * half[n] + c + subj_c[subj[n]]));
}