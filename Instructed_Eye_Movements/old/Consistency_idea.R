#### check spread idea ####

#### packages ####
library(tidyverse)

#### load data ####
load("scratch/models_df")

#### constants ####
# Screen distance
Screen_dist <- 54
# pixels per cm
ppcm <- 1920/54

#### any functions ####
get_VisDegs <- function(separation,distance){
  ((2*atan2(separation,(2*distance)))*180)/pi
}


#### sort out data ####
# make column for condition 
df$cond <- "baseline"
df$cond[df$second_half == 1 & df$given_instruction == 0] <- "practice"
df$cond[df$second_half == 0 & df$given_instruction == 1] <- "instructed"
df$cond[df$second_half == 1 & df$given_instruction == 1] <- "transfer"


#### Consistency score ####
#### CS: All three boxes ####
# this is for consistency across all three boxes

# participant, condition, separation
pcs <- df %>%
  group_by(participant, cond, separation) %>% 
  count(fixated_box) %>%
  mutate(prop = n/sum(n)) %>%
  select(-n) %>%
  spread(fixated_box, prop) %>%
  replace(is.na(.), 0) %>%
  summarise(consistency = max(`1`,`2`,`3`) - min(`1`,`2`,`3`)) %>%
  ungroup() %>%
  ggplot(aes(separation, consistency, colour = cond)) + 
  geom_point() +
  facet_wrap(~participant)
pcs
  

# participant, condition
pc <- pcs$data %>%
  group_by(participant, cond) %>%
  summarise(consistency = mean(consistency))

#### CS: Centre vs side ####
pcs_2 <- df %>%
  group_by(participant, cond, separation) %>%
  count(centre) %>%
  mutate(prop = n/sum(n)) %>%
  select(-n) %>%
  spread(centre, prop) %>%
  replace(is.na(.), 0) %>%
  summarise(consistency = max(`0`,`1`) - min(`0`,`1`)) %>%
  ungroup() %>%
  ggplot(aes(separation, consistency, colour = cond)) + 
  geom_point() +
  facet_wrap(~participant)
pcs_2

# get averages of this 
pc_2 <- pcs_2$data %>%
  group_by(participant, cond) %>%
  summarise(consistency = mean(consistency))

#### simply side proportion ####
# need to add in standardised seps for this to work... 
for(p in unique(df$participant)){
  df$standard[df$participant == p] <- as.numeric(as.factor(df$separation[df$participant == p]))
}

# tidy 
rm(p)

# add in overall group membership
plt_side <- df %>% 
  group_by(participant, separation, second_half, given_instruction) %>%
  summarise(centre = mean(centre)) %>%
  mutate(Side_prop = 1 - centre,
         Half = second_half + 1,
         Half = as.factor(Half)) %>%
  ggplot(aes(get_VisDegs(separation/ppcm,Screen_dist), Side_prop, colour = Half)) + 
  geom_point() + 
  theme_bw() + 
  theme(strip.text.x = element_blank()) + 
  ggthemes::scale_colour_ptol() + 
  facet_wrap(~given_instruction + participant, ncol = 6)
plt_side$labels$y <- "Proportion of Fixations to the Side boxes"
plt_side$labels$x <- "Delta (Visual Degrees)" 
plt_side$labels$colour <- "Half"
plt_side

