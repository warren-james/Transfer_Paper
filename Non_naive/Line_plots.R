#### Make line plots for our data ####
# This is from testing us
# This script is to create plots for the transfer paper 

#### load libraries ####
library(tidyverse)
library(gridExtra)
#library(gridBase)

#### any functions ####

#### any constants ####

#### load in data ####
# get Throwing
load("Throwing/scratch/predicted_accuracy")

pred_dat_Throw <- df

# tidy 
rm(df)

# now detection 
load("Detection/scratch/accuracy_data")

pred_dat_Det <- accuracy_data

# tidy 
rm(accuracy_data)


#### make data the same so it can be lined up ####
# Detection
plt_dat_det <- pred_dat_Det %>%
  group_by(participant, acc_type) %>%
  summarise(Accuracy = mean(accuracy))

# sort colnames
colnames(plt_dat_det) <- c("Participant",
                           "Acc_type",
                           "Accuracy")

# add in column 
plt_dat_det$Task <- "Detection"

plt_dat_det$Acc_type <- as.factor(plt_dat_det$Acc_type)

# Throwing 
plt_dat_Throw <- pred_dat_Throw %>% 
  group_by(Participant, Acc_type) %>%
  summarise(Accuracy = mean(Accuracy))

# add in column 
plt_dat_Throw$Task <- "Throwing"

# bind together 
temp <- as.data.frame(plt_dat_det)
temp1 <- as.data.frame(plt_dat_Throw)
plt_dat <- rbind(temp1, temp)

plt_dat <- as.tibble(plt_dat)

# tidy 
rm(plt_dat_det, plt_dat_Throw, temp, temp1)

# now only keep levels we want 
plt_dat <- plt_dat[plt_dat$Acc_type == "Optimal" | 
                     plt_dat$Acc_type == "Actual",]

plt_dat$Task <- as.factor(plt_dat$Task)
plt_dat$Task <- factor(plt_dat$Task, levels(plt_dat$Task)[c(2,1)])


#### plots: together ####
plt <- ggplot(plt_dat, aes(Acc_type, Accuracy))
plt <- plt + theme_bw()
plt <- plt + geom_point()
plt <- plt + geom_line(aes(group = Participant))
# plt <- plt + theme(strip.background = element_blank(),
#                    strip.text.x = element_blank(),
#                    text = element_text(size = 10))
# plt <- plt + scale_y_continuous(limits = c(.5, 1))
plt <- plt + facet_wrap(~Task, scales = "free_y", ncol = 1)
plt$labels$x <- "Accuracy Type"
plt

ggsave("Extra_plots/line_plot.png", width = 3, height = 5)

# save this 
# ggsave("Extra_plots/line_plot.png")


#### plots: make them separate them put together... ####
# separate data 
plt_dat_Throw <- plt_dat[plt$dat$Task == "Throwing",]
plt_dat_Det <- plt_dat[plt_dat$Task == "Detection",]

# try dropping unused levels 
plt_dat_Throw$Acc_type <- droplevels(plt_dat_Throw$Acc_type)
plt_dat_Det$Acc_type <- droplevels(plt_dat_Det$Acc_type)

# rename levels for plotting 
levels(plt_dat_Det$Acc_type) <- c("Actual", "Optimal")
levels(plt_dat_Throw$Acc_type) <- c("Actual", "Optimal")

# make plots 
# throwing
plt_t <- ggplot(plt_dat_Throw, aes(Acc_type, Accuracy))
plt_t <- plt_t + theme_bw()
plt_t <- plt_t + geom_point()
plt_t <- plt_t + geom_line(aes(group = Participant))
plt_t <- plt_t + scale_y_continuous(limits = c(0,1),
                                    breaks = c(0, .2, .4, .6, .8, 1),
                                    expand = c(0,0))
plt_t <- plt_t + theme(axis.title.x = element_blank(),
                       axis.title.y = element_blank())
plt_t <- plt_t + facet_wrap(~Task)
#plt_t$labels$x <- "Accuracy Type"

# detection 
plt_d <- ggplot(plt_dat_Det, aes(Acc_type, Accuracy))
plt_d <- plt_d + theme_bw()
plt_d <- plt_d + geom_point()
plt_d <- plt_d + geom_line(aes(group = Participant))
plt_d <- plt_d + scale_y_continuous(limits = c(0.5,1),
                                    expand = c(0,0))
plt_d <- plt_d + theme(axis.title.y = element_blank(),
                       axis.title.x = element_blank())
plt_d <- plt_d + facet_wrap(~Task)
#plt_d$labels$x <- "Accuracy Type"

# combine these 
grid.arrange(plt_t, plt_d, nrow = 2,
             bottom = "Accuracy Type",
             left = "Accuracy")

# save 
ggsave("Extra_plots/lines_new_scales.png")


#### Analyses ####
# should be able to use plotting data... as it's a t.test anyway 
# First do throwing 
t_test_Throw <- t.test(plt_dat_Throw$Accuracy ~ plt_dat_Throw$Acc_type)

t_test_Det <- t.test(plt_dat_Det$Accuracy ~ plt_dat_Det$Acc_type)

# maybe try a correlation
# need to reshape data set unfortunately 
wide_throw <- plt_dat_Throw %>%
  spread(Acc_type, Accuracy)

wide_det <- plt_dat_Det %>%
  spread(Acc_type, Accuracy)

# now do correlations
cor.test(wide_throw$Actual, wide_throw$Optimal)

cor.test(wide_det$Actual, wide_det$Optimal)

# both significant... should this be used instead? 

