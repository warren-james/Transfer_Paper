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

blank_dat <- data.frame(
  Accuracy = c(0, 1, 0.5, 1), 
  Acc_type = "Actual",
  Task = rep(c("Throwing", "Detection"), each = 2))

scaleFUN <- function(x) sprintf("%.2f", x)

#### plots: together ####
plt <- ggplot(plt_dat, aes(Acc_type, Accuracy))
plt <- plt + theme_bw()
plt <- plt + geom_point()
plt <- plt + geom_blank(data = blank_dat)
plt <- plt + geom_line(aes(group = Participant))
# plt <- plt + theme(strip.background = element_blank(),
#                    strip.text.x = element_blank(),
#                    text = element_text(size = 10))
plt <- plt + scale_y_continuous(expand = c(0,0), labels=scaleFUN)
plt <- plt + facet_wrap(~Task, scales = "free_y", ncol = 2)
plt$labels$x <- "Accuracy Type"
plt

ggsave("Extra_plots/line_plot.png", width = 4, height = 3)





(plt_dat %>% 
  spread(Acc_type, Accuracy) %>%
  mutate(difference = Optimal - Actual) -> diff_in_acc)
