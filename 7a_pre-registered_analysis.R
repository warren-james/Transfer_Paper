#### Analysis #### 
# Level 4 Thesis by Elle
# 2017/18
# Written by Warren James
# Script to carry out the planned analyses 

#### Libraries ####
library(tidyverse)

#### any functions #### 
get_VisDegs <- function(separation,distance){
  ((2*atan2(separation,(2*distance)))*180)/pi
}

#### Constants ####
Screen_dist <- 53
ppcm <- 1920/54

#### Extra information ####
# I think there were 36 pixels per degree... Will have to check though
pixVD = 0.036
options(digits = 4)

#### load in data ####
load("scratch/switch_df")

# reduce columns down to what we need 
df <- select(switch_df,
             participant,
             condition,
             separation, 
             part,
             half,
             block,
             separation,
             fixated_box,
             correct,
             opt_acc,
             exp_acc) 

# tidy 
rm(switch_df)

#### ANALYSES ####
#### ANALYSES: Difference in Accuracy scores ####
# Get a difference score for second half sessions for both groups. Then compare these together.
# Smaller values means closer to the optimal accuracy level.

# first set up data frame 
dat_analysis_1 <- df %>%
  group_by(participant, condition, half) %>%
  summarise(Act_Accuracy = mean(correct),
            Opt_Accuracy = mean(opt_acc),
            Exp_Accuracy = mean(exp_acc))

# make some variabls factors 
dat_analysis_1$condition <- as.factor(dat_analysis_1$condition)
dat_analysis_1$half <- as.factor(dat_analysis_1$half)

# Get difference score 
dat_analysis_1$difference <- dat_analysis_1$Opt_Accuracy - dat_analysis_1$Act_Accuracy

# now we only want the second half 
dat_analysis_1 <- dat_analysis_1[dat_analysis_1$half == "second",]

# now do t-test 
t_test_diff <- t.test(dat_analysis_1$difference ~ dat_analysis_1$condition)






#### PLOTS ####
#### PLOT: Morvan and Maloney (2012) figure 6-like plot ####
# not sure where to start with this one... but we'll figure it out 











