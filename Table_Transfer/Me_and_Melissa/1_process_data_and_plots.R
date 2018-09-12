#### This script is for double blind experiment ####
# has data for the reaching and throwing task for each paritipant
# No need to worry about switch points for this data
# They either did a nothing or the reaching task before the throwing task 

#### Notes ####
# for Order: 0 = unprimed. 1 = primed 
# Position is centred on 0 

#### libraries ####
library(tidyverse)
library(ggthemes)
#### Any functions ####

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
plt <- plt + geom_point()
plt <- plt + theme_bw()
plt <- plt + facet_wrap(~Order + Participant, ncol = 8)
plt <- plt + scale_x_continuous(limits = c(1,7))
plt <- plt + theme_bw()
plt <- plt + scale_colour_ptol()
plt <- plt + theme(strip.background = element_blank(),
                   strip.text.x = element_blank())
plt <- plt + theme(legend.position = "none")
plt <- plt + xlab("delta (metres)")
plt <- plt + ylab("normalised standing position")
plt

# save this 
ggsave("scratch/plots/double_blind.png", width = 8, height = 4)
 

#NB: unprimed comes first
 
#### Analyses ####
#### Analyses: t.tests... ugh ####
# first just a simple t.test of position 
t_test_dat <- df[df$Hoop.dist == 13 | df$Hoop.dist == 5,]

# try just large hoops 
large <- t_test_dat[t_test_dat$Hoop.dist == "13",]
small <- t_test_dat[t_test_dat$Hoop.dist == "5",]

# now do the t.test
# large
t.test(large$Participant.pos ~ large$Order)

# small 
t.test(small$Participant.pos ~ small$Order)

# try lm?
anova(lm(Participant.pos ~ Hoop.dist + Order,
         data = t_test_dat))

# try difference value 
small <- select(small,
                Participant, 
                Order, 
                Hoop.dist, 
                Participant.pos)

colnames(small) <-  c("Participant",
                      "Order",
                      "Close_hoop",
                      "Participant.Pos")

small <- small %>%
  group_by(Participant, Order, Close_hoop) %>%
  summarise(avg_pos_close = mean(Participant.Pos))

large <- select(large,
                Participant, 
                Order, 
                Hoop.dist, 
                Participant.pos)

colnames(large) <-  c("Participant",
                      "Order",
                      "Far_hoop",
                      "Participant.Pos")

large <- large %>%
  group_by(Participant, Order, Far_hoop) %>%
  summarise(avg_pos_far = mean(Participant.Pos))


# This is what Mellissa did 
Both <- merge(small, large)
Both$Difference <- Both$avg_pos_far - Both$avg_pos_close

# tidy 
rm(small, large)

t.test(Both$Difference ~ Both$Order)
# gives the same results as mellisa

#### remake box_plots with this data ####
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

#### Analyses: glm? ####
# might need to remove points above 1 if we want to do this properly?
glm_dat <- df[df$Participant.pos <= 1,]

# run first model with just hoop distance
# NB: CAREFUL, check how to incorporate continuous variables
# - Also want to include random effects but it doesn't like it
m1 <- glm(Participant.pos ~ Hoop.dist, 
          data = glm_dat, family = binomial())

# now add in order
m1.1 <- glm(Participant.pos ~ Hoop.dist + Order, 
            data = glm_dat, family = binomial())

# Try using glmer as well to get random effects 
m2 <- glmer(Participant.pos ~ Hoop.dist + (1|Participant),
            data = glm_dat, family = binomial())




