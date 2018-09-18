#### on our data ####
# same as 1_sim_dat but on real data 

#### libraries ####
library(tidyverse)
library(rstan)
library(psych)

#### load data ####
load("../Instructed_Eye_Movements/scratch/models_df")

# listify for passing to Stan 
stan_df <- list(
  N = nrow(df),
  acc = df$correct,
  delta = df$sep_scaled
)

# run model 
m1 <- stan(
  file = "exp3_m1.stan", 
  data = stan_df,
  chains = 1,
  warmup = 1000,
  iter = 2000,
  refresh = 100
)

# make dumb plot 
# extract samples
post_m1 <- rstan::extract(m1)

b <- mean(post_m1$b)
c <- mean(post_m1$c)

# sort data 
acc_dat <- df %>%
  group_by(sep_scaled) %>%
  summarise(accuracy = mean(correct)) %>%
  mutate(p = logistic(pmax(0,b*sep_scaled+c)))

# good(ish) plot, bad model
plot(acc_dat$sep_scaled, acc_dat$accuracy)
lines(acc_dat$sep_scaled, acc_dat$p)

# for a better plot
get_fx_for_sep <- function(d, post) {
  print(delta)
  fx <- tibble(
    condition = rep(c(
      "Baseline", 
      "Instructed", 
      "Practice",
      "Transfer"), each = length(post$a)),
    delta = d,
    samples = c(
      logistic(post$a + post$b * d), 
      logistic(post$a + post$instruction + post$b  * d + post$instrbysep * d), 
      logistic(post$a + post$half + post$b  * d + post$halfbysep  * d),
      logistic(post$a + post$instruction + post$half + post$halfbyinst + post$b * d + post$instrbysep * d + post$halfbysep * d)))
  
  return(fx)
}






