#### Instructed eye movements #####
# this is just to make the new plots using optimal performance compared to actual accuracy 

#### libraries needed ####
library(tidyverse)
library(reshape2)
library(psyphy)

#### any functions #### 
get_VisDegs <- function(separation,distance){
  ((2*atan2(separation,(2*distance)))*180)/pi
}

#### Constants ####
Screen_dist <- 53
ppcm <- 1920/54

#### load in data ####
# all data
load("scratch/switch_df")

# Just accuracy data
load("scratch/Elle_acc_dat") 
Elle_accuracy_data <- ungroup(Elle_accuracy_data)

# fixations 
load("scratch/Elle_fix_data")

#### Make plots ####
#### PLOT: Accuracy ####
# setup data 
Acc_Actual <- Elle_accuracy_data[Elle_accuracy_data$acc_type == "Actual",]
Acc_Optimal <- Elle_accuracy_data[Elle_accuracy_data$acc_type == "Optimal",]

# need to reorder the levels 
Acc_Actual$condition <- as.factor(Acc_Actual$condition)
Acc_Optimal$condition <- as.factor(Acc_Optimal$condition)

# reorder them 
levels(Acc_Actual$condition) <- factor(Acc_Actual$condition, 
                                       levels(Acc_Actual$condition)[c(2,1)])

levels(Acc_Optimal$condition) <- factor(Acc_Optimal$condition, 
                                        levels(Acc_Optimal$condition)[c(2,1)])

# plot
plt <- ggplot(Acc_Actual[Acc_Actual$half == "second",],
              aes(offset_from_sp,
                  Accuracy))
plt <- plt + theme_bw()
plt <- plt + geom_point()
plt <- plt + geom_line(data = Acc_Optimal,
                       aes(offset_from_sp,
                           Accuracy),
                       colour = "red")
plt <- plt + theme(strip.background = element_blank(),
                   strip.text.x = element_blank(),
                   panel.border = element_blank())
plt <- plt + facet_wrap(~condition + participant)
plt$labels$x <- "Offset from Switch Point (Visual Degrees)"
plt
# # save 
# ggsave("scratch/plots/Part_2_ActVsOptAccuracy.png")

# tidy 
rm(plt, Acc_Optimal, Acc_Actual)

#### PLOT: Fxations ####
# setup data
Fix_Actual <- Elle_fix_data[Elle_fix_data$fix_type == "Actual" & 
                              Elle_fix_data$box == "Side",]

plt <- ggplot(Fix_Actual[Fix_Actual$half == "second",],
              aes(offset_from_sp,
                  mean_fix_pos,
              colour = as.factor(participant)))
plt <- plt + theme_bw()
plt <- plt + geom_line()
plt <- plt + theme(legend.position="none",
                   panel.border = element_blank())
plt$labels$x <- "Offset from Switch Point (Visual Degrees)"
plt$labels$y <- "Proportion of Fixations to the side"
# save 
# ggsave("scratch/plots/Part_2_ActFixProps.png")

#### Try some line plots of overall accuracy #### 
plt_dat_lines <- Elle_accuracy_data %>%
  group_by(participant, half, condition, acc_type) %>%
  summarise(Accuracy = mean(Accuracy))

# make plot for second session: expected vs. optimal
plt_lines <- ggplot(plt_dat_lines[plt_dat_lines$acc_type != "Actual" & 
                                    plt_dat_lines$half == "second",],
                    aes(acc_type, Accuracy))
plt_lines <- plt_lines + theme_bw()
plt_lines <- plt_lines + geom_point()
plt_lines <- plt_lines + geom_line(aes(group = participant))
plt_lines <- plt_lines + facet_wrap(~condition)
plt_lines$labels$x <- "Accuracy Type"
plt_lines

# save 
ggsave("scratch/plots/line_plot.png")



### new added 2023
plt_dat_lines %>%
  pivot_wider(names_from = "acc_type", values_from = "Accuracy") %>%
  mutate(half = if_else(half=="first", "first session", "second session"),
         condition = if_else(condition == "Instructions", "Primed", "Control")) %>%
  ggplot(aes(Optimal, Actual, colour = condition)) + geom_point() + 
  facet_wrap(~half) + geom_abline() + 
  coord_equal() + 
  scale_x_continuous("optimal accuracy", limits = c(0.7, 0.9)) + 
  scale_y_continuous("expected accuracy", limits = c(0.7, 0.9)) + 
  theme_bw() + 
  ggthemes::scale_color_ptol()

ggsave("scatter_plot.png", width = 6, height = 3)

