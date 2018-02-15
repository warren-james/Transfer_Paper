#### Plotting Script #### 
# Level 4 Thesis by Elle
# 2017/18
# Written by Warren James
# Script used to make plots of proportion of fixations 
# made to the centre or side box(es)

#### libraries needed ####
library(tidyverse)

#### load in data ####
load("scratch/Elle_switch_nar_data")

#### separate datasets ####
# this part is so we have a separate dataset for:
# no instructions/first half 
no_inst_1 <- switch_df[switch_df$condition == "No_instructions" & 
                         switch_df$block < 5,]

# no instructions/second half 
no_inst_2 <- switch_df[switch_df$condition == "No_instructions" & 
                         switch_df$block > 4,]

# instructions/tutorial
inst_tut <- switch_df[switch_df$condition == "Instructions" & 
                        switch_df$part == 3,]

# instructions/task
inst_tas <- switch_df[switch_df$condition == "Instructions" & 
                        switch_df$part == 2,]

#### Assuming we only want to look at part 2 for each participant ####
df_part2 <- switch_df[switch_df$part == 2,]

#### make plots ####
# setup data.frame/tibble for plots
# centre proportions
temp <- group_by(df_part2, participant,separation,condition)
centre_prop <- summarise(temp, prop_fixated = mean(centre))
centre_prop$box <- "centre"

# side proportions
side_prop <- summarise(temp, prop_fixated = 1 - mean(centre))
side_prop$box <- "side"

# tidy
rm(temp)

# merge data
plt_dat <- rbind(centre_prop, side_prop)

# tidy 
rm(centre_prop, side_prop)

# need to add switch point data back in 
temp <- group_by(df_part2, participant, condition)
switch_points <- summarise(temp, switch_point = unique(switch_point))

# tidy 
rm(temp)

# now to make the plots 
prop_plt <- ggplot(data = plt_dat, 
                   aes(x = separation,
                       y = prop_fixated))
prop_plt <- prop_plt + geom_area(aes(colour = box,
                                     fill = box),
                                 position = "stack")
prop_plt <- prop_plt + geom_vline(data = switch_points,
                                  aes(xintercept = as.numeric(switch_point)), 
                                  linetype = "dashed")
prop_plt <- prop_plt + facet_wrap(~condition + participant)
prop_plt

# save plot 
# ggsave("scratch/proportions_plot.pdf", width = 10, height = 10)

#### boxplots of accuracy ####
# make boxplot data
temp <- group_by(switch_df, participant, condition)
box_dat <- summarise(temp, correct = mean(correct))

# tidy 
rm(temp)

# make plots 
box_plt <- ggplot(box_dat, 
                   aes(condition, 
                       correct))
box_plt <- box_plt + geom_boxplot()

#### compare: first half and tutorial ####
fh_tut <- rbind(inst_tut, no_inst_1)

# set data for boxplts 
temp <- group_by(fh_tut,
                 participant,
                 condition)
ftut_box_dat <- summarise(temp,
                          correct = mean(correct))

# tidy 
rm(temp)

# make plots 
ftut_box_plt <- ggplot(ftut_box_dat, 
                       aes(condition, 
                           correct))
ftut_box_plt <- ftut_box_plt + geom_boxplot()

#### compare: second half and task ####
sh_tas <- rbind(inst_tas, no_inst_2)

# set data for boxplts 
temp <- group_by(sh_tas,
                 participant,
                 condition)
stas_box_dat <- summarise(temp,
                          correct = mean(correct))

# tidy 
rm(temp)

# make plots 
stas_box_plt <- ggplot(stas_box_dat, 
                       aes(condition, 
                           correct))
stas_box_plt <- stas_box_plt + geom_boxplot()

rm(list = ls())
