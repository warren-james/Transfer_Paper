#### Analysis #### 
# Level 4 Thesis by Elle
# 2017/18
# Written by Warren James
# Script to carry out the planned analyses 

#### Libraries ####
library(tidyverse)
library(gridExtra)
library(ggthemes)

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

#### ANALYSIS ####
#### ANALYSIS: Difference session 2 ####

# NB: For all the t.tests, you should put var.equal = T so as to use the standard student's t-test
# This stops it correcting the df otherwise it applies the Welch correction which is useful when the 
# group sizes are unequal. 

# Get a difference score for second half sessions for both groups. Then compare these together.
# Smaller values means closer to the optimal accuracy level.

# first set up data frame 
dat_analysis <- df %>%
  group_by(participant, condition, half) %>%
  summarise(Act_Accuracy = mean(correct),
            Opt_Accuracy = mean(opt_acc),
            Exp_Accuracy = mean(exp_acc))

# make some variabls factors 
dat_analysis$condition <- as.factor(dat_analysis$condition)
dat_analysis$half <- as.factor(dat_analysis$half)

# Get difference score 
dat_analysis$difference <- dat_analysis$Opt_Accuracy - dat_analysis$Act_Accuracy

# now we only want the second half 
dat_analysis_1 <- dat_analysis[dat_analysis$half == "second",]

# now do t-test 
t_test_diff_1 <- t.test(dat_analysis_1$difference ~
                          dat_analysis_1$condition,
                        alternative = c("less"),
                        var.equal = T)
# not sig, but we know that from the bayesian regression model 

# Try with expected?
dat_analysis_1$difference_2 <- dat_analysis_1$Opt_Accuracy - dat_analysis_1$Exp_Accuracy

# try test again 
t_test_diff_1_1 <- t.test(dat_analysis_1$difference_2 ~ 
                            dat_analysis_1$condition,
                          # alternative = c("greater"),
                          var.equal = T)

#### ANALYSIS: Difference for session 1 ####
# same as above, but for first session 
dat_analysis_2 <- dat_analysis[dat_analysis$half == "first",]

# now test 
t_test_diff_2 <- t.test(dat_analysis_2$difference ~
                          dat_analysis_2$condition,
                        # alternative = c("less"),
                        var.equal = T)
# This is significant though...


#### ANALYSIS: Practice effects ####
# just use "No_instructions" group and compare accross halves 
dat_analysis_3 <- dat_analysis[dat_analysis$condition == "No_instructions",]

# now test 
t_test_prac <- t.test(dat_analysis_3$difference ~
                        dat_analysis_3$half,
                      # alternative = c("greater"),
                      var.equal = T)
# This isn't sig... so no real practice effects present
# but the bayes model shows a better picture anyway 

#### PLOTS ####
#### PLOT: Morvan and Maloney (2012) figure 6-like plot ####
# so, make it split by session with groups split by colour 
# At the moment it's using actual performance instead of
# expected accuracy as this is based on a different calculation 
# of accuracy over distance to what participants actually  did

# setup data
plt_data <- df %>%
  group_by(participant, condition, half) %>%
  summarise(Actual = mean(correct),
            Optimal = mean(opt_acc))

# make them factors 
plt_data$condition <- as.factor(plt_data$condition)
plt_data$half <- as.factor(plt_data$half)

# sort out level names 
levels(plt_data$half) <- c("First Session", "Second Session")
levels(plt_data$condition) <- c("Primed", "Control")

# reorder condition 
plt_data$condition <- factor(plt_data$condition,
                             levels(plt_data$condition)[c(2,1)])

# can use Expected instead of Actual... but due to a slight difference
# in accuracy calculations for the switch points, this would
# make it look like some participants weren't as good as optimal...
# plot
plt <- ggplot(plt_data, aes(Optimal, Actual,
                            colour = condition))
plt <- plt + theme_bw()
plt <- plt + scale_y_continuous(limits = c(0.6, 0.9)) +
  scale_x_continuous(limits = c(0.6, 0.9))
plt <- plt + geom_abline(intercept = 0, slope = 1)
plt <- plt + geom_point()
plt <- plt + facet_wrap(~ half)
plt <- plt + scale_color_ptol()
# plt <- plt + theme(legend.position = "none")
plt$labels$colour <- "Condition"
plt <- plt + xlab("optimal accuracy")
plt <- plt + ylab("actual accuracy")
ggsave("scatter_plots.png", width = 4, height = 3)
plt

#### PLOTS: Clarke and Hunt (2016) figure 3 ####
# just using second half with proportion of fixations to side box
# split by group/condition 
# reload switch_df
load("scratch/switch_df")

# setup data frame 
plt2_data <- switch_df %>%
  group_by(participant, condition, half, separation, standard_sep, offset_from_sp) %>%
  summarise(Centre = mean(centre))

plt2_data$Side <- 1 - plt2_data$Centre

# sort out factors 
plt2_data$condition <- as.factor(plt2_data$condition)
# plt2_data$half <- as.factor(plt2_data$half)

# rename levels
levels(plt2_data$condition) <- c("Primed", "Control")

# reorder condition 
plt2_data$condition <- factor(plt2_data$condition,
                              levels(plt2_data$condition)[c(2,1)])

# reduce to only second half 
plt2_data <- plt2_data[plt2_data$half == "second",]

# plot 
plt2 <- ggplot(plt2_data, aes(get_VisDegs(separation/ppcm, Screen_dist),
                              Side, 
                              colour = condition))
plt2 <- plt2 + theme_bw()
plt2 <- plt2 + geom_point()
plt2 <- plt2 + geom_line(aes(group = participant))
plt2 <- plt2 + facet_wrap(~condition)
plt2 <- plt2 + theme(legend.position = "none")
plt2 <- plt2 + scale_color_ptol()
plt2 <- plt2 + xlab("proportion of fixations to a side box")
plt2 <- plt2 + ylab("delta (visual degrees)")
plt2$labels$colour <- "Condtion"
ggsave("line_plots.png", width = 4, height = 3)

#### PLOTS: Combine the above two? ####
# grid.arrange(plt, plt2, nrow = 2)

# need to save this manually


