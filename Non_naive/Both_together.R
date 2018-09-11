#### Putting second session plots together ####
# retrieve the data, make the plots, put them side by side 

#### Libraries ####
library(tidyverse)
library(gridExtra)

#### Any functions ####
# get visual degrees
get_VisDegs <- function(separation,distance){
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
side_fixations <- mutate(side_fixations, vis_deg = get_VisDegs(separation/ppcm, Screen_dist))

dot_plt <- ggplot(side_fixations, aes(vis_deg, prop_fixated))
dot_plt <- dot_plt + theme_bw()
dot_plt <- dot_plt + facet_wrap(~participant, nrow = 1)
dot_plt <- dot_plt + geom_path(data = opt_fixations,
                               colour = "darkblue",
                               aes(get_VisDegs(separation/ppcm, Screen_dist),
                                   fix_locations),
                               size = 0.5)
dot_plt <- dot_plt + geom_point()
dot_plt <- dot_plt + theme(strip.background = element_blank(),
                           strip.text.x = element_blank())
dot_plt <- dot_plt + xlab("delta (in visual degrees)")
dot_plt <- dot_plt + ylab("proportion of fixations \n to the side boxes")
ggsave("detection_task.pdf", width = 8, height = 2.5)
ggsave("detection_task.png", width = 8, height = 2.5)


#### PLOTS: THROWING TASK ####
plt <- ggplot(df_part2, aes(Hoop_Pos*slab_size, 
                            Abs_Pos))
#plt <- plt + geom_jitter(size = 1.2)
plt <- plt + geom_path(data = Opt_Pos,
                       colour = "darkblue",
                       aes(Slab*slab_size, 
                           Opt_Pos),
                       size = 0.5)
plt <- plt + geom_point(alpha = 1/15)
plt <- plt + scale_x_continuous("delta (metres)", limits = c(0,13))
plt <- plt + ylab("normalised \n standing position")
plt <- plt + theme_bw()
plt <- plt + theme(strip.background = element_blank(),
                   strip.text.x = element_blank(),
                   text = element_text(size = 12))
plt <- plt + facet_wrap(~Participant, nrow = 1)
ggsave("throwing_task.pdf", width = 8, height = 2.5)
ggsave("throwing_task.png", width = 8, height = 2.5)
