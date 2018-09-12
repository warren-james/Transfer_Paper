library(tidyverse)
library(ggthemes)
library(viridisLite)
dat = read_csv('results.csv')

dat$Participant = as.factor(dat$Participant)
dat$Order = as.factor(dat$Order)
levels(dat$Order) = c('control', 'primed')
names(dat)[2] <- "group"
names(dat)[8] <- "hoop_dist"
names(dat)[9] <- "participant_position"
names(dat)[10] <- "hit"

# Work out who did the Reaching Task properly
dat$ReachingCorrect = ((dat$Centre == 1) + (dat$Middle == 1) + (dat$Edge==1))==3
# aggregate(ReachingCorrect ~ Participant+Order, dat, FUN="mean")


# normalise participant position
dat$standing_pos = abs(dat$participant_position / dat$hoop_dist)


# take the close and far hoops
(dat %>% 
	# filter(hoop_dist %in% c(5, 13)) %>%
	group_by(Participant, group, hoop_dist) %>%
	summarise(accuracy = mean(hit)) %>%
	select(-Participant)) -> acc_dat

# acc_dat$hoop <- fct_relevel(acc_dat$hoop, close, medium, far)

opt_dat <- tibble(group = "optimal", hoop_dist = 13, accuracy = rbinom(32, 15, 0.5)/15, hoop = "far")

acc_dat <- bind_rows(acc_dat, opt_dat)


acc_dat$hoop <- as.factor(acc_dat$hoop_dist)
levels(acc_dat$hoop) <- c("close", "medium", "far")

acc_dat$group <- as_factor(acc_dat$group)

plt <- ggplot(acc_dat, aes(x = hoop, y = accuracy, fill = group))
plt <- plt + geom_boxplot()
plt <- plt + theme_bw()
plt <- plt + scale_fill_ptol()
plt <- plt + scale_y_continuous(limits = c(0, 1), expand = c(0, 0))
ggsave("acc_bloxplot.png", width = 4, height = 3)