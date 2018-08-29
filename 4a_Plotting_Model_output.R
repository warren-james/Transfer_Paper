#### Plotting Outputs from script 4 ####
# Takes the models and plots them...

#### Libraries needed ####
library(tidyverse)
library(rethinking)
library(viridisLite)

#### Any Functions ####
get_hpdi_region_from_samples <- function(m, post, ln = TRUE) {
  pred_data <- list(
    participant = rep(1:length(unique(df$participant)), each = 4),
    given_instruction =   rep(c(0, 1, 0, 1), length(unique(df$participant))),
    second_half = rep(c(0, 0, 1, 1), length(unique(df$participant))))
  
  mu <- link(m, data = pred_data)
  mu.PI <- apply(mu, 2, PI)
  
  pred_data$lower <- mu.PI[1,]
  pred_data$upper <- mu.PI[2,]
  
  #pred_data$targ_pr <- as.factor(pred_data$targ_pr)
  #levels(pred_data$targ_pr) <- c("absent", "present")
  
  if (ln == TRUE) { 
    pred_data$lower <- exp(pred_data$lower)
    pred_data$upper <- exp(pred_data$upper)
  }
  
  names(pred_data)[1] <- "participant"
  pred_data <- as.data.frame(pred_data)
  return(pred_data)
}

#### Load data ####
load("scratch/models_df")

#### First model ####
# load model
# instructions + half + halfbyinst
# random intercepts ONLY 
load("scratch/models/m1_rand_intercept")

# get samples 
post <- extract.samples(m1)

# get model lines 
model_lines <- get_hpdi_region_from_samples(m1, post, FALSE)

# try this for model_lines$participant 
# seems to be the only way to fix this issue
sequence <- c(1,10:19,2,20:24,3:9)
model_lines$participant <- rep(sequence, each=4)

# need summary data for plot
plt_dat <- df %>%
  group_by(participant, given_instruction, second_half) %>%
  summarise(accuracy = mean(correct))

# now make a plot this 
# just half
plt <- ggplot()
plt <- plt + theme_bw()
plt <- plt + geom_ribbon(data = model_lines, 
                         aes(x = second_half, ymin = lower, ymax = upper),
                         alpha = 0.5)
plt <- plt + geom_point(data = plt_dat, 
                        aes(x = second_half, y = accuracy),
                        shape = 3, alpha = 0.8, show.legend = FALSE) 
plt <- plt + facet_wrap(~participant)
plt

# now under instructions 
plt <- ggplot()
plt <- plt + theme_bw()
plt <- plt + geom_ribbon(data = model_lines,
                         aes(given_instruction, ymin = lower, ymax = upper),
                         alpha = 0.5)
plt <- plt + geom_point(data = plt_dat, 
                        aes(given_instruction, accuracy, colour = second_half),
                        shape = 3, alpha = 0.8, show.legend = T)
plt <- plt + theme(legend.position = "bottom")
plt <- plt + facet_wrap(~participant)
plt

#### how about we try some box plots ####


