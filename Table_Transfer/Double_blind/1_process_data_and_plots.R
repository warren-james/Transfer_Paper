#### This script is for double blind experiment ####
# has data for the reaching and throwing task for each paritipant
# No need to worry about switch points for this data
# They either did nothing or the reaching task before the throwing task 

#### Notes ####
# for Order: 0 = unprimed. 1 = primed 
# Position is centred on 0 

#### Packages ####
library(tidyverse)
library(ggthemes)

#### Constants ####
slab_size <- 0.46

#### load in Data ####
df <- read.csv("data/results.csv")

# make participant a factor 
df$Participant <- as.factor(df$Participant)

# sort out levels for Order 
df$Order <- as.factor(df$Order)
levels(df$Order) <- c("Control", "Primed", "Optimal")

# Normalise position 
df$Participant.pos <- abs(df$Participant.pos/df$Hoop.dist)

#### make plots ####
plt <- ggplot(df, aes(Hoop.dist*slab_size, Participant.pos,
                      colour = Order))
plt <- plt + geom_point(alpha = 0.4)
plt <- plt + theme_bw()
plt <- plt + facet_wrap(~Order + Participant, ncol = 8)
plt <- plt + scale_x_continuous(limits = c(1,7))
plt <- plt + theme_bw()
plt <- plt + scale_colour_ptol()
plt <- plt + theme(strip.background = element_blank(),
                   strip.text.x = element_blank())
plt <- plt + theme(legend.position = "none")
plt <- plt + xlab("Delta (Metres)")
plt <- plt + ylab("Normalised Standing Position")
plt

# save this 
ggsave("scratch/plots/double_blind.png", width = 8, height = 4)
 
#NB: unprimed comes first
 
#### Analyses ####
#### Analyses: Standing position ####
# first just a simple t.test of position 
t_test_dat <- df[df$Hoop.dist == 13 | df$Hoop.dist == 5,]

# sort data for t.test 
t_test_dat <- t_test_dat %>%
  group_by(Participant, Order, Hoop.dist) %>%
  summarise(Participant.pos = mean(Participant.pos))

# try just large hoops 
large <- t_test_dat[t_test_dat$Hoop.dist == "13",]
small <- t_test_dat[t_test_dat$Hoop.dist == "5",]

# now do the t.test
# large
t.test(large$Participant.pos ~ large$Order)

# small 
t.test(small$Participant.pos ~ small$Order)

# tidy 
rm(small, large)

#### Analyses: Difference scores ####
diff_test <- t_test_dat %>%
  group_by(Participant, Order) %>%
  spread(Hoop.dist, Participant.pos) %>%
  setNames(c("Partiticpant", "Order", "Close_hoop", "Far_hoop"))

# calculate difference scores
diff_test$Difference <- diff_test$Far_hoop - diff_test$Close_hoop 

# test difference
t.test(diff_test$Difference ~ diff_test$Order,
       var.equal = T)

#### Box_plots for difference ####
# make plot 
plt <- ggplot(Both, aes(Order, Difference,
                        colour = Order))
plt <- plt + geom_boxplot()
plt <- plt + theme_bw()
plt <- plt + theme(legend.position = "none")
plt <- plt + scale_y_continuous(limits = c(-0.4, 1))
plt$labels$y <- "Change in Standing Position"
plt$labels$x <- "Group"
plt

# save 
ggsave("scratch/plots/box_plot.png")
