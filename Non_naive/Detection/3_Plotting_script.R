#### For Josephine ####

#### libraries needed ####
library(tidyverse)

#### Any Functions ####
# function to get VisDegs 
get_VisDegs <- function(x,y){
  (2*atan2(x,(2*y)))*(180/pi)
}
ppcm <- 1920/54


#### Constants ####
Screen_dist <- 53

#### load in data ####
load("scratch/switch_nar_data")

#### Assuming we only want to look at part 2 for each participant ####
df_part2 <- switch_df[switch_df$part == 2,]

#### make plots ####
# setup data.frame/tibble for plots
# centre proportions
temp <- group_by(df_part2, participant,separation)
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

# Add in block to look at this
temp <- group_by(df_part2, participant,separation, block)
centre_prop <- summarise(temp, prop_fixated = mean(centre))
centre_prop$box <- "centre"

# side proportions
side_prop <- summarise(temp, prop_fixated = 1 - mean(centre))
side_prop$box <- "side"

# tidy
rm(temp)

# merge data
plt_dat_blk <- rbind(centre_prop, side_prop)

# tidy 
rm(centre_prop, side_prop)

# need to add switch point data back in 
temp <- group_by(df_part2, participant)
switch_points <- summarise(temp, switch_point = unique(switch_point))

# tidy 
rm(temp)

#### Stacked plots ####
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
if(length(unique(plt_dat$participant)) > 1){
  prop_plt <- prop_plt + facet_wrap(~participant)
}
prop_plt

# with block separation
prop_plt_blk <- ggplot(data = plt_dat_blk, 
                      aes(x = separation,
                          y = prop_fixated))
prop_plt_blk <- prop_plt_blk + geom_area(aes(colour = box,
                                     fill = box),
                                 position = "stack")
prop_plt_blk <- prop_plt_blk + geom_vline(data = switch_points,
                                  aes(xintercept = as.numeric(switch_point)), 
                                  linetype = "dashed")
if(length(unique(plt_dat_blk)) > 1){
  prop_plt_blk <- prop_plt_blk + facet_grid(block~participant)
} else {
  prop_plt_blk <- prop_plt_blk + facet_wrap(~block)
}
prop_plt_blk

#### dot plots ####
side_fixations <- plt_dat[plt_dat$box == "side",]
side_fixations_blk <- plt_dat_blk[plt_dat_blk$box == "side",]

# make plots
prop_dot <- ggplot(data = side_fixations, 
                   aes(x = separation,
                       y = prop_fixated))
prop_dot <- prop_dot + geom_point()
prop_dot <- prop_dot + geom_vline(data = switch_points,
                                  aes(xintercept = as.numeric(switch_point)), 
                                  linetype = "dashed")
if(length(unique(side_fixations$participant)) > 1){
  prop_dot <- prop_dot + facet_wrap(~participant)
}
prop_dot <- prop_dot + theme_bw()
prop_dot <- prop_dot + scale_x_continuous(name = "Separation in pixels")
prop_dot <- prop_dot + scale_y_continuous(name = "Proportion of fixations to one of the side boxes")
prop_dot
# save this 
ggsave("scratch/plots/Part2.pdf")


# with block separation
prop_dot_blk <- ggplot(data = side_fixations_blk, 
                       aes(x = separation,
                           y = prop_fixated))
prop_dot_blk <- prop_dot_blk + geom_point()
prop_dot_blk <- prop_dot_blk + geom_vline(data = switch_points,
                                          aes(xintercept = as.numeric(switch_point)), 
                                          linetype = "dashed")
if(length(unique(side_fixations_blk)) > 1){
  prop_dot_blk <- prop_dot_blk + facet_grid(block~participant)
} else {
  prop_dot_blk <- prop_dot_blk + facet_wrap(~block)
}
prop_dot_blk <- prop_dot_blk + theme_bw()
prop_dot_blk



#### PLOTS: w/ visual degrees? ####
side_fixations$separation_deg <- get_VisDegs(as.numeric(side_fixations$separation)/ppcm, Screen_dist)
switch_points$switch_point_deg <- get_VisDegs(as.numeric(switch_points$switch_point)/ppcm, Screen_dist)

# make plot
prop_dot <- ggplot(data = side_fixations, 
                   aes(x = separation_deg,
                       y = prop_fixated))
prop_dot <- prop_dot + geom_point()
prop_dot <- prop_dot + geom_vline(data = switch_points,
                                  aes(xintercept = as.numeric(switch_point_deg)), 
                                  linetype = "dashed")
if(length(unique(side_fixations$participant)) > 1){
  prop_dot <- prop_dot + facet_wrap(~participant)
}
prop_dot <- prop_dot + theme_bw()
prop_dot <- prop_dot + scale_x_continuous(name = "Delta (Visual Angle)")
prop_dot <- prop_dot + scale_y_continuous(name = "Proportion of fixations to one of the side boxes")
prop_dot

# save 
ggsave("scratch/plots/Part2_VA.pdf")


#### Make plot for Amelia ####
# sort out line(s)
# setup separations
seps <- seq(min(side_fixations$separation), max(side_fixations$separation), 0.5)

# setup new dataframe
opt_fixations <- tibble(participant = character(),
                        separation = numeric(),
                        fix_location = numeric())

for(i in unique(switch_points$participant)){
  d <- switch_points[switch_points$participant == i,]
  for(x in seps){
    if(x < as.numeric(d$switch_point)){
      fl <- 0
    } else if(x > as.numeric(d$switch_point)){
      fl <- 1
    }
    opt_fixations <- rbind(opt_fixations, data.frame(participant = i,
                                                     separation = x,
                                                     fix_locations = fl))
  }
}

# save this file 
save(opt_fixations, file = "scratch/opt_fixations")

# make plot 
dot_plt <- ggplot(side_fixations, aes(get_VisDegs(separation/ppcm, Screen_dist), 
                                 prop_fixated))
dot_plt <- dot_plt + geom_point()
dot_plt <- dot_plt + theme_bw()
dot_plt <- dot_plt + facet_wrap(~participant)
dot_plt <- dot_plt + geom_path(data = opt_fixations,
                               colour = "blue",
                               aes(get_VisDegs(separation/ppcm, Screen_dist),
                                   fix_locations),
                               size = 0.15)
dot_plt <- dot_plt + theme(strip.background = element_blank(),
                           strip.text.x = element_blank())
dot_plt$labels$x <- "Delta (in Visual Degrees)"
dot_plt$labels$y <- "Proportion of Fixations to the side boxes"
dot_plt


# save this 
ggsave("scratch/plots/Part_2_plots.png")

# save side_fixations too 
save(side_fixations, file = "scratch/side_fixations")

#### Need to check the below part works... ####









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
