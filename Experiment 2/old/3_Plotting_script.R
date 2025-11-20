#### Plotting Script #### 
# Script used to make plots of proportion of fixations 
# made to the centre or side box(es)

#### libraries needed ####
library(tidyverse)

#### Any functions ####
# Gets Visual Degrees
get_VisDegs <- function(separation,distance){
  ((2*atan2(separation,(2*distance)))*180)/pi
}

#### Any constants ####
Screen_dist <- 54.4
ppcm <- 1920/54

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
plt_dat <- df_part2 %>%
  group_by(participant, separation, condition) %>%
  summarise(centre_prop = mean(centre),
            side_prop = 1 - mean(centre)) %>%
  gather(4:5,
         key = box,
         value = prop_fixated) %>%
  separate(box, c("box", "remove"), "_") %>%
  select(-remove)

# need to add switch point data back in 
switch_points <- df_part2 %>%
  group_by(participant, condition) %>%
  summarise(switch_point = unique(switch_point))

# now to make the plots 
# prop_plt <- ggplot(data = plt_dat, 
#                    aes(x = separation,
#                        y = prop_fixated))
# prop_plt <- prop_plt + geom_area(aes(colour = box,
#                                      fill = box),
#                                  position = "stack")
# prop_plt <- prop_plt + geom_vline(data = switch_points,
#                                   aes(xintercept = as.numeric(switch_point)), 
#                                   linetype = "dashed")
# prop_plt <- prop_plt + facet_wrap(~condition + participant)
# prop_plt

# save plot 
# ggsave("scratch/plots/proportions_plot.pdf", width = 10, height = 10)

# dots version 
side_fix_dat <- plt_dat[plt_dat$box == "side",]

#### Make switch line again ####
seps <- seq(min(df_part2$separation), max(df_part2$separation), 0.5)

switch_line <- tibble(participant = character(), 
                      condition = character(),
                      separation = numeric(),
                      Fixated_box = numeric())

#### REALLY SLOW ####
# Is there a quicker way to do this?
# create the lines
for(p in unique(df_part2$participant)){
  switch <- unique(switch_points$switch_point[switch_points$participant == p])
  switch <- as.numeric(switch)
  c <- unique(df_part2$condition[df_part2$participant == p])
  
  # go through sequence
  for(s in seps){
    if(s < switch){
      fl <- 0
    } else if(s > switch){
      fl <- 1
    }
    switch_line <- rbind(switch_line, data.frame(participant = p,
                                                 condition = c,
                                                 separation = s,
                                                 Fixated_box = fl))
  }
}

# tidy 
rm(fl,p, s, seps, switch)

#### Plot: Dot plot ####
# first make side_fix_dat$condition a factor 
side_fix_dat$condition <- as.factor(side_fix_dat$condition)

# change level names 
levels(side_fix_dat$condition) <- c("Primed", "Unprimed")

# reorder this
side_fix_dat$condition <- factor(side_fix_dat$condition,
                                 levels(side_fix_dat$condition)[c(2,1)])

# same for switch_line
levels(switch_line$condition) <- c("Unprimed", "Primed")

# might not be needed... but it looks nice...
# maybe supplementary? Can't remember
# plt
dot_plt <- ggplot(side_fix_dat, aes(get_VisDegs(separation/ppcm, Screen_dist),
                                    prop_fixated,
                                    colour = condition))
dot_plt <- dot_plt + geom_point()
dot_plt <- dot_plt + geom_line(data = switch_line,
                               aes(get_VisDegs(separation/ppcm, Screen_dist),
                                   Fixated_box))
dot_plt <- dot_plt + theme_bw()
# dot_plt <- dot_plt + geom_path(data = switch_line,
#                                colour = "black",
#                                aes(get_VisDegs(separation/ppcm, Screen_dist),
#                                    Fixated_box),
#                                size = 0.15)
dot_plt <- dot_plt + facet_wrap(~condition + participant, ncol = 6)
dot_plt <- dot_plt + theme(strip.background = element_blank(),
                           strip.text.x = element_blank(),
                           legend.position = "bottom")
dot_plt$labels$x <- "Delta (in Visual Degrees)"
dot_plt$labels$y <- "Proportion of fixations to a side box"
dot_plt$labels$colour <- "Condition"
dot_plt

# save 
# ggsave("scratch/plots/Part2_dots_wcolour.png")

# tidy 
rm(plt_dat, side_fix_dat)

#### Make plots for paper ####
switch_df$half <- "First"
switch_df$half[switch_df$block > 4] <- "Second"

# setup new data frame 
side_prop <- switch_df %>% 
  group_by(participant, separation, condition, half) %>%
  summarise(prop_fixated = 1 - mean(centre))

# sort side_prop condition labels 
side_prop$condition <- as.factor(side_prop$condition)
levels(side_prop$condition) <- c("Primed", "Unprimed")

# reorder these levels 
side_prop$condition <- factor(side_prop$condition,
                              levels(side_prop$condition)[c(2,1)]) 

# make plot 
dot_plt <- ggplot(side_prop, aes(get_VisDegs(separation/ppcm, Screen_dist), 
                                 prop_fixated))#,
                                 #colour = half))
dot_plt <- dot_plt + geom_point(aes(shape = half,
                                    colour = half)) + 
                     scale_shape_manual(values=c(3,4))
dot_plt <- dot_plt + theme_bw()
dot_plt <- dot_plt + facet_wrap(~condition + participant, ncol = 6)
dot_plt <- dot_plt + geom_path(data = switch_line,
                               colour = "blue",
                               aes(get_VisDegs(separation/ppcm, Screen_dist),
                                   Fixated_box),
                               size = 0.15)
dot_plt <- dot_plt + theme(strip.background = element_blank(),
                           strip.text.x = element_blank())
dot_plt$labels$x <- "Delta (in Visual Degrees)"
dot_plt$labels$y <- "Proportion of Fixations to the side boxes"
dot_plt$labels$shape <- "Half"
dot_plt$labels$colour <- "Half"
dot_plt

# save this 
ggsave("scratch/plots/Part_2_plots.pdf")


#### boxplots of accuracy ####
# make boxplot data
box_dat <- switch_df %>%
  group_by(participant, condition, half) %>%
  summarise(correct = mean(correct))

# make plots 
box_plt <- ggplot(box_dat, 
                   aes(condition, 
                       correct,
                       colour = half))
box_plt <- box_plt + geom_boxplot()
box_plt$labels$x <- "Condition"
box_plt$labels$y <- "Accuracy"
box_plt$labels$colour <- "Half"
box_plt


