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


#### how about we try some box plots ####
box_plt_dat <- tibble(participant = numeric(),
                      cond_type = numeric(),
                      samples = numeric())

for(ii in 1:24){
  participant <- rep(ii, length(post$a_p[,ii])*4)
  fx <- tibble(
    cond_type = rep(c(
      "Baseline", 
      "Instructed", 
      "Practice",
      "Transfer"), each = length(post$a_p[,ii])),
    samples = c(
      logistic(post$a + post$a_p[,ii]), 
      logistic(post$a + post$a_p[,ii] + post$instruction), 
      logistic(post$a + post$a_p[,ii] + post$half),
      logistic(post$a + post$a_p[,ii] + post$instruction + post$half + post$halfbyinst)))
  
  temp <- cbind(participant, fx)
  box_plt_dat <- rbind(box_plt_dat, temp)
}

# tidy 
rm(temp, fx, ii, participant)

# now make plot... might work 
# make things factors though 
box_plt_dat$cond_type <- as.factor(box_plt_dat$cond_type)

# try adding in the real data 
# first setup coding 
df$cond_type <- 0 
df$cond_type[df$second_half == 0 &
               df$given_instruction == 0] <- "Baseline"
df$cond_type[df$second_half == 1 &
               df$given_instruction == 0] <- "Practice"
df$cond_type[df$second_half == 0 &
               df$given_instruction == 1] <- "Instructed"
df$cond_type[df$second_half == 1 &
               df$given_instruction == 1] <- "Transfer"

box_dat_real <- df %>%
  group_by(participant, cond_type) %>% 
  summarise(samples = mean(correct))


# sort out participant order in sim data
num_reps <- length(post$a_p[,1])*4
box_plt_dat$participant <- rep(sequence, each = num_reps)


# box_dat_real$participant <- rep(sequence, each = 2)

#### PLOTS: Makes box plot ####
# plot
plt_box <- ggplot(box_plt_dat, aes(cond_type, samples,
                                   fill = cond_type))
plt_box <- plt_box + theme_bw() 
plt_box <- plt_box + geom_boxplot()
plt_box <- plt_box + theme(legend.position = "bottom",
                           strip.background = element_blank(),
                           strip.text.x = element_blank(),
                           axis.text.x = element_blank(),
                           axis.ticks.x = element_blank(),
                           axis.title.x = element_blank())
plt_box <- plt_box + facet_wrap(~participant, ncol = 6)
plt_box <- plt_box + geom_point(data = box_dat_real, 
                                aes(cond_type, samples),
                                size = 2)
plt_box <- plt_box + geom_point(data = box_dat_real,
                                aes(cond_type, samples,
                                    colour = cond_type),
                                size = 1)
plt_box$labels$y <- "Accuracy"
plt_box$labels$colour <- "Condition"
plt_box$labels$fill <- "Condition"
plt_box 

# save
ggsave("scratch/plots/Bayes_box_plots.png",
       height = 12,
       width = 16,
       units = "cm")




