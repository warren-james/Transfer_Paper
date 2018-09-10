#### Putting second session plots together ####
# retrieve the data, make the plots, put them side by side 

#### Libraries ####
library(tidyverse)
library(gridExtra)

#### Any functions ####
# get visual degrees
get_VisDegs <- function(separationc,distance){
  (2*atan2(separation,(2*distance)))*(180/pi)
}

#### Any constants ####
# Detection
ppcm <- 1920/54
Screen_dist <- 53

# Throwing
slab_size <- 0.46

#### Load data ####
# Detection first 
# switch point data 
load("Detection/scratch/opt_fixations")

# dataset 
load("Detection/scratch/side_fixations")

# Throwing 
# switch points 
load("Throwing/scratch/Opt_Pos")

# dataset
load("Throwing/scratch/df_part2")


#### PLOTS ####
#### PLOTS: DETECTION TASK ####
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


#### PLOTS: THROWING TASK ####
plt <- ggplot(df_part2, aes(Hoop_Pos*slab_size, 
                            Abs_Pos))
#plt <- plt + geom_jitter(size = 1.2)
plt <- plt + geom_point(alpha = 1/15)
plt <- plt + geom_path(data = Opt_Pos,
                       colour = "blue",
                       aes(Slab*slab_size, 
                           Opt_Pos),
                       size = 0.2)
plt <- plt + scale_x_continuous(limits = c(0,13))
plt <- plt + theme_bw()
plt <- plt + theme(strip.background = element_blank(),
                   strip.text.x = element_blank(),
                   text = element_text(size = 12))
plt <- plt + facet_wrap(~Participant)
plt$labels$x <- "Delta (metres)"
plt$labels$y <- "Normalised Standing Position"
plt

#### PLOTS: COMBINE ####
grid.arrange(dot_plt, plt, ncol = 2)

# need to save manually...
# haven't figured out the command for that yet






