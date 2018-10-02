#### on our data ####
# same as 1_sim_dat but on real data 

#### libraries ####
library(tidyverse)
library(rstan)
library(psych)

#### load data ####
load("../Instructed_Eye_Movements/scratch/models_df")

#### acc ~ delta + inst ####
# listify for passing to Stan 
stan_df <- list(
  N = nrow(df),
  inst = df$given_instruction,
  acc = df$correct,
  delta = df$sep_scaled
)

# run model 
m2 <- stan(
  file = "models/exp3_m2.stan", 
  data = stan_df,
  chains = 1,
  warmup = 1000,
  iter = 2000,
  refresh = 100
)

# save model 
save(m2, file = "scratch/model_outputs/m2")

# make dumb plot 
# extract samples
post_m2 <- rstan::extract(m2)

b <- mean(post_m2$b)
b_i <- mean(post_m2$b_i)
c <- mean(post_m2$c)

# sort data 
acc_dat <- df %>%
  group_by(sep_scaled, given_instruction) %>%
  summarise(accuracy = mean(correct)) %>%
  mutate(p = 0.5 +  logistic(pmax(0,
                                  b*sep_scaled+
                                    b_i*given_instruction+c))/2)

# plot 
# probably go for ggplot here
plot(acc_dat$sep_scaled, acc_dat$accuracy)
lines(acc_dat$sep_scaled, acc_dat$p)

#### acc ~ detla + (inst + half)^2 ####
stan_df <- list(
  N = nrow(df),
  inst = df$given_instruction,
  acc = df$correct,
  delta = df$sep_scaled,
  half = df$second_half
)

# run model 
m3 <- stan(
  file = "models/exp3_m3.stan", 
  data = stan_df,
  chains = 1,
  warmup = 1000,
  iter = 2000,
  refresh = 100
)

# save model 
save(m3, file = "scratch/model_outputs/m3")

# extract samples 
post_m3 <- rstan::extract(m3)

# this one underestimates performance at the closest separations

# try this again with a slight difference 
# removed the 2 * line_pred -1
# run model 
m3_1 <- stan(
  file = "models/exp3_m3_1.stan", 
  data = stan_df,
  chains = 1,
  warmup = 1000,
  iter = 2000,
  refresh = 100
)

# save model 
save(m3_1, file = "scratch/model_outputs/m3_1")

# this one drops below 50% accuracy

#### acc ~ (inst + half)^2 ####
stan_df <- list(
  N = nrow(df),
  inst = df$given_instruction,
  acc = df$correct,
  half = df$second_half
)

# run model 
m4 <- stan(
  file = "models/exp3_m4.stan", 
  data = stan_df,
  chains = 1,
  warmup = 1000,
  iter = 2000,
  refresh = 100
)

# save model 
save(m4_1, file = "scratch/model_outputs/m4")

#### acc ~ (inst + half)^2 w/random intercepts ####
stan_df <- list(
  N = nrow(df),
  inst = df$given_instruction,
  acc = df$correct,
  half = df$second_half,
  S = length(unique(df$participant)),
  subj = df$participant
)

# run model
m5 <- stan(
  file = "models/exp3_m5.stan", 
  data = stan_df,
  chains = 1,
  warmup = 1000,
  iter = 2000,
  refresh = 100
)

# save 
save(m5, file = "scratch/model_outputs/m5")

#### acc ~ delta + inst + half /w random intercepts ####
stan_df <- list(
  N = nrow(df),
  inst = df$given_instruction,
  acc = df$correct,
  delta = df$sep_scaled,
  half = df$second_half,
  S = length(unique(df$participant)),
  subj = df$participant
)

# run model
m6 <- stan(
  file = "models/exp3_m6.stan", 
  data = stan_df,
  chains = 4,
  cores = 4,
  warmup = 1000,
  iter = 4000,
  refresh = 100
)

# save 
save(m6, file = "scratch/model_outputs/m6")




