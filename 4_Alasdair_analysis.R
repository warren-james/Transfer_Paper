#### Alasdair's Analysis
#### libraries needed ####
library(tidyverse)
library(rethinking)
library(ggthemes)


fit_model <- function(df) {
  m <- map2stan( 
    alist(
      correct ~ dbinom(1, p),
      logit(p) <- a + a_p[participant] + b * given_instruction,
      a ~ dnorm(0, 1),
      b ~ dnorm(0, 1),
      a_p[participant] ~ dnorm(0, sigma_p),
      sigma_p ~ dcauchy(0, 1)),
    data = df)
  
  return(m)
  
}

plot_indiv <- function(df, m, fname) {
  # extract things from model
  # first, we get each person's fit
  pred_data <- list(
    participant = rep(1:length(unique(df$participant)), each = 2),
    given_instruction =   rep(c(0, 1), length(unique(df$participant))))
  
  mu <- link(m, data = pred_data)
  mu.PI <- apply(mu, 2, PI)
  pred_data$lower <- mu.PI[1,]
  pred_data$upper <- mu.PI[2,]
  pred_data$condition <- ifelse(
    pred_data$given_instruction == 1, "Instructions", "No_instructions")
  pred_data <- as.data.frame(pred_data)
  
  
  #  now we get the fixed effects
  
  # plot what we got!
  
  acc_df <- aggregate(data = df, correct ~ participant + condition, "mean")
  
  plt <- ggplot(x = as.numeric(acc_df$participant))
  plt <- plt + geom_point(data = acc_df, aes(x = participant, y = correct, colour = condition))
  # add model details!
  plt <- plt + geom_errorbar(data = pred_data, aes(x = participant, ymin = lower, ymax = upper, colour = condition))
  # format nicely
  plt <- plt + theme_solarized()  + scale_colour_solarized("blue")
  plt <- plt + scale_y_continuous(limits = c(0,1), expand = c(0,0), breaks = c(0, 0.5, 1))
  plt
  ggsave(fname)
  
}


plot_fix_ef <- function(m, fname) {
  # plot effect of Instructions
  post <- extract.samples(m)
  
  fx <- tibble(
    condition = rep(c("No_instructions", "Instructions", "Difference"), each = length(post$a)),
    samples = c(logistic(post$a), logistic(post$a + post$b),  logistic(post$a + post$b) - logistic(post$a)) )
  
  plt <- ggplot(fx, aes(x = samples, fill = condition))
  plt <- plt + geom_density(alpha = 0.5)
  plt <- plt + theme_solarized()  + scale_fill_solarized("blue")
  plt <- plt + scale_x_continuous(
    name = "probablity of correctly responding to target", limits = c(-0.2,1), expand = c(0,0))
  ggsave(fname)
}


# load in the data set 
load("scratch/Elle_switch_nar_data")
df <- switch_df
rm(switch_df)

#  tidy up
df$switch_point <- as.numeric(df$switch_point)


df$given_instruction <- as.numeric(df$condition == "Instructions")

# take part 2 for the main comparison


# fit model
sdf <- filter(df, block <= 4)
m <- fit_model(sdf)
plot_indiv(sdf, m, "scratch/first_four_blocks_indiv.png")
plot_fix_ef(m, "scratch/first__four_blocks_effect.png")


# fit model
sdf <- filter(df, block > 4)
m <- fit_model(sdf)
plot_indiv(sdf, m, "scratch/second_four_blocks_indiv.png")
plot_fix_ef(m, "scratch/second__four_blocks_effect.png")