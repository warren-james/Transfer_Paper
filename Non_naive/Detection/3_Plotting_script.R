#### Non-naive participants ####

#### libraries needed ####
library(tidyverse)

#### Any Functions ####
# function to get VisDegs 
get_VisDegs <- function(x,y){
  (2*atan2(x,(2*y)))*(180/pi)
}

#### Constants ####
Screen_dist <- 53
ppcm <- 1920/54

#### load in data ####
load("scratch/switch_nar_data")

#### Assuming we only want to look at part 2 for each participant ####
df_part2 <- switch_df[switch_df$part == 2,]

#### make plots ####
# setup data.frame/tibble for plots
plt_dat <- df_part2 %>%
  group_by(participant, separation) %>%
  summarise(centre_prop = mean(centre),
            side_prop = 1 - mean(centre)) %>%
  gather(3:4, 
         key = box, 
         value = prop_fixated) %>%
  separate(box, c("box", "remove"), "_") %>%
  select(-remove)

# Add in block to look at this
plt_dat_blk <- df_part2 %>%
  group_by(participant, separation, block) %>%
  summarise(centre_prop = mean(centre),
            side_prop = 1 - mean(centre)) %>%
  gather(4:5, 
         key = box, 
         value = prop_fixated) %>%
  separate(box, c("box", "remove"), "_") %>%
  select(-remove)

# need to add switch point data back in 
switch_points <- df_part2 %>%
  group_by(participant) %>%
  summarise(switch_point = unique(switch_point))

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
prop_dot <- ggplot(data = side_fixations, 
                   aes(x = get_VisDegs(as.numeric(separation)/ppcm, Screen_dist),
                       y = prop_fixated))
prop_dot <- prop_dot + geom_point()
prop_dot <- prop_dot + geom_vline(data = switch_points,
                                  aes(xintercept = get_VisDegs(as.numeric(switch_point)/ppcm, Screen_dist)), 
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


#### Make plot for Paper ####
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
