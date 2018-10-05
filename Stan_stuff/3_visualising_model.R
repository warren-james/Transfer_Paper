#### on our data ####
# make plots for the models fitted in script 2
#### libraries ####

library(tidyverse)
library(rstan)
library(psych)

#### m3: acc ~ detla + (inst + half)^2 ####
#### load data ####
load("../Instructed_Eye_Movements/scratch/models_df")
load("scratch/model_outputs/m3")

# extract samples
post_samples <- as.tibble(rstan::extract(m3))

# for a better plot
get_fx_for_sep <- function(d, post) {
  # get fixed effects of model
	# d is the scaled separtations we want to predict for
  fx <- tibble(
    condition = rep(c(
      "Baseline", 
      "Instructed", 
      "Practice",
      "Transfer"), each = length(post$c)),
    delta = d,
    samples = c(
      plogis(post$c + post$b * d), 
      plogis(post$c + post$b_i + post$b * d), 
      plogis(post$c + post$b_h + post$b * d),
      plogis(post$c + post$b_i + post$b_h + post$b_hi + post$b * d )))
  
  return(fx)
}

fx_min_sep  <- get_fx_for_sep(min(df$sep_scaled), post_samples)
fx_mean_sep <- get_fx_for_sep(mean(df$sep_scaled), post_samples)
fx_max_sep  <- get_fx_for_sep(max(df$sep_scaled), post_samples)

# bind these
fx <- bind_rows(fx_min_sep, fx_mean_sep, fx_max_sep)

# tidy 
rm(fx_max_sep, fx_mean_sep, fx_min_sep)

plt <- ggplot(fx, aes(x = samples, fill = condition))
plt <- plt + geom_density(alpha = 0.5)
plt <- plt + theme_bw()  + scale_fill_viridis_d()
plt <- plt + facet_wrap(~delta)
plt <- plt + scale_x_continuous(
  name = "probablity of correctly responding to target", limits = c(0.2,1), expand = c(0,0))

# ggsave("scratch/plots/joint_model.png", width = 8, height = 3)


#### m3_1: acc ~ (inst + half)^2 ####
load("scratch/model_outputs/m3_1")

# extract samples
post_samples <- as.tibble(rstan::extract(m3_1))

# for a better plot
get_fx_for_sep <- function(post) {
  # get fixed effects of model
  # d is the scaled separtations we want to predict for
  fx <- tibble(
    condition = rep(c(
      "Baseline", 
      "Instructed", 
      "Practice",
      "Transfer"), each = length(post$c)),
    samples = c(
      plogis(post$c), 
      plogis(post$c + post$b_i), 
      plogis(post$c + post$b_h),
      plogis(post$c + post$b_i + post$b_h + post$b_hi)))
  
  return(fx)
}

fx <- get_fx_for_sep(post_samples)

plt <- ggplot(fx, aes(x = samples, fill = condition))
plt <- plt + geom_density(alpha = 0.5)
plt <- plt + theme_bw()  + scale_fill_viridis_d()
# plt <- plt + facet_wrap(~delta)
plt <- plt + scale_x_continuous(
  name = "probablity of correctly responding to target", limits = c(0.5,1), expand = c(0,0))

# simple checks 
# desc_dat <- df %>%
#   group_by(given_instruction, second_half) %>%
#   summarise(accuracy = mean(correct)) %>%
#   ungroup()
# 
# desc_dat$condition <- c("Baseline", 
#                          "Practice",
#                          "Instructed",
#                          "Transfer")
# 
# desc_dat$type <- c("Real_data")
# 
# desc_dat <- select(desc_dat,
#                    condition,
#                    type,
#                    accuracy)
# 
# desc_sim <- fx %>%
#   group_by(condition) %>%
#   summarise(accuracy = mean(samples))
# 
# desc_sim$type <- c("Sim_data")
# 
# # compare groups 
# avg_acc <- rbind(desc_dat, desc_sim)

#### m6: acc ~ (half + inst + delta)^3 /w random intercepts #### 
#### NB: need to figure out how to include random intercept ####
# for each or else it looks funny...
load("scratch/model_outputs/m6")

# extract samples
# at the moment, this ignores the random interecepts which 
# makes it not that good...
post_samples <- extract(m6)

post_samples <- tibble(c = post_samples$c,
                       b_d = post_samples$b_d,
                       b_i = post_samples$b_i,
                       b_h = post_samples$b_h,
                       b_hi = post_samples$b_hi,
                       b_hd = post_samples$b_hd,
                       b_id = post_samples$b_id,
                       b_hid = post_samples$b_hid)

# for a better plot
get_fx_for_sep <- function(d, post) {
  # get fixed effects of model
  # d is the scaled separtations we want to predict for
  fx <- tibble(
    condition = rep(c(
      "Baseline", 
      "Instructed", 
      "Practice",
      "Transfer"), each = length(post$c)),
    delta = d,
    samples = c(
      plogis(post$c +
             post$b_d * d), 
      plogis(post$c +
             post$b_i +
             post$b_d * d), 
      plogis(post$c +
             post$b_h +
             post$b_d * d),
      plogis(post$c +
             post$b_i +
             post$b_h +
             post$b_hi +
            (post$b_hid + post$b_d) * d )))
  
  return(fx)
}

# get effects
fx_min_sep  <- get_fx_for_sep(min(df$sep_scaled), post_samples)
fx_mean_sep <- get_fx_for_sep(mean(df$sep_scaled), post_samples)
fx_max_sep  <- get_fx_for_sep(max(df$sep_scaled), post_samples)

# bind these
fx <- bind_rows(fx_min_sep, fx_mean_sep, fx_max_sep)

# tidy 
rm(fx_max_sep, fx_mean_sep, fx_min_sep)

# make plot 
plt <- ggplot(fx, aes(x = samples, fill = condition, frame = delta))
plt <- plt + geom_density(alpha = 0.5, aes(frame = delta))
plt <- plt + theme_bw()  + scale_fill_viridis_d()
plt <- plt + facet_wrap(~ delta)
plt <- plt + scale_x_continuous(
  name = "probablity of correctly responding to target", limits = c(0.2,1), expand = c(0,0))
# plt <- plt + transition_time(delta)


