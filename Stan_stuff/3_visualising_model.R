#### on our data ####
# make plots for the models fitted in script 2
#### libraries ####

library(tidyverse)
library(rstan)
library(psych)

#### load data ####
load("../Instructed_Eye_Movements/scratch/models_df")
load("scratch/models/m3")


# make dumb plot 
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
      0.5 + 0.5 * plogis(post$c + post$b * d), 
      0.5 + 0.5 * plogis(post$c + post$b_i + post$b * d), 
      0.5 + 0.5 * plogis(post$c + post$b_h + post$b * d),
      0.5 + 0.5 * plogis(post$c + post$b_i + post$b_h + post$b * d )))
  
  return(fx)
}

fx_min_sep  <- get_fx_for_sep(min(df$sep_scaled), post_samples)
fx_mean_sep <- get_fx_for_sep(mean(df$sep_scaled), post_samples)
fx_max_sep  <- get_fx_for_sep(max(df$sep_scaled), post_samples)

fx <- bind_rows(fx_min_sep, fx_mean_sep, fx_max_sep)

plt <- ggplot(fx, aes(x = samples, fill = condition))
plt <- plt + geom_density(alpha = 0.5)
plt <- plt + theme_bw()  + scale_fill_viridis_d()
plt <- plt + facet_wrap(~delta)
plt <- plt + scale_x_continuous(
  name = "probablity of correctly responding to target", limits = c(0.2,1), expand = c(0,0))

ggsave("scratch/plots/joint_model.png", width = 8, height = 3)

