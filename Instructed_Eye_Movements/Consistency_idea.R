#### check spread idea ####

#### packages ####
library(tidyverse)

#### load data ####
load("scratch/models_df")

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
  summarise(consistency = max(`1`,`2`,`3`) - min(`1`,`2`,`3`))
  

# participant, condition
pc <- pcs %>%
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
  summarise(consistency = max(`0`,`1`) - min(`0`,`1`))

# get averages of this 
pc_2 <- pcs_2 %>%
  group_by(participant, cond) %>%
  summarise(consistency = mean(consistency))




