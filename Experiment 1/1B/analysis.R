library(tidyverse)
library(haven)
library(ggthemes)
library(brms)
library(patchwork)
library(tidybayes)


theme_set(theme_bw())


dat <- read_sav("data/DATACOLLECTIONPHYSICAL.sav") 

(dat %>%
	mutate(person = as.factor(1:nrow(dat))) %>%
	select(person, Condition, 
		PositionCHoopTrial1, PositionCHoopTrial2, PositionCHoopTrial3,
		PositionMHoopTrial1, PositionMHoopTrial2, PositionMHoopTrial3,
		PositionFHoopTrial1, PositionFHoopTrial2, PositionFHoopTrial3
		) %>%
	rename(condition = "Condition") %>%
	gather("hoop", "standing_position", -condition, -person) %>%
	mutate(
		condition = as_factor(condition),
		hoop = as_factor(hoop),
		hoop = fct_collapse(hoop, 
			near = c("PositionCHoopTrial1", "PositionCHoopTrial2", "PositionCHoopTrial3"),
			med   = c("PositionMHoopTrial1", "PositionMHoopTrial2", "PositionMHoopTrial3"),
			far   = c("PositionFHoopTrial1", "PositionFHoopTrial2", "PositionFHoopTrial3"))
		)) -> dat 

dat$distance <- as.numeric(dat$hoop)
dat$distance[which(dat$distance == 2)] <- 4.5
dat$distance[which(dat$distance == 3)] <- 7.0

levels(dat$condition) <- c("Reaching Task", "Logic Puzzles", "Maths Questions")

dat %>% 
	mutate(standing_position = standing_position / distance) -> dat

summary(dat)


plt <- ggplot(dat, aes(x = distance, y = standing_position, colour = condition))
plt <- plt + geom_point(alpha = 0.5)
plt <- plt + facet_wrap(~person, nrow = 3)
plt <- plt + scale_colour_manual(values = c("#CC6677", "#4477AA", "#117733"))
plt <- plt + theme_bw()
plt <- plt + scale_x_continuous("Delta (metres)")
plt <- plt + scale_y_continuous("Normalised Standing Position")
plt

ggsave('standing_position.pdf', width = 8, height = 6)
ggsave('standing_position.png', width = 8, height = 6)


my_priors <- c(prior(normal(0, 0.5), class = sd),
               prior(normal(-1.5, 1), class = b),
               prior(normal(0, 0.5), class = sd, dpar = "hu"),
               prior(normal(0, 1.5), class = b, dpar = "hu"))

dat <- dat %>% filter(hoop != "med")

# first, prior...
m <- brm(bf(standing_position ~ 0 + hoop + (0 + hoop | person),
            hu ~ 0 + hoop + (0 + hoop | person)), 
         data = dat,
         family = hurdle_lognormal(),
         prior = my_priors,
         iter = 5000,
         backend = "cmdstanr",
         sample_prior = "only")

m %>% gather_draws(`[b|hu]_.*`, regex = TRUE) %>%
  mutate(param = if_else(str_detect(.variable, "hu"), "hu", "b"),
         .variable = str_remove(.variable, "b_(hu_)*"),
         .variable = str_remove_all(.variable, "hoop|condition")) %>%
  rename(hoop = ".variable") %>%
  mutate(hoop = as_factor(hoop), 
         hoop = fct_relevel(hoop, "near"),
         group = "prior") -> prior

write_csv(prior, "1_prior.csv")

### now do post

m <- brm(bf(standing_position ~ 0 + hoop:condition + (0 + hoop:condition | person),
            hu ~ 0 + hoop:condition + (0 + hoop:condition | person)), 
         data = dat,
         family = hurdle_lognormal(),
         prior = my_priors,
         iter = 5000,
         backend = "cmdstanr")

##############
# plot predictions
##############
m %>% gather_draws(`[b|hu]_.*`, regex = TRUE) %>%
  mutate(param = if_else(str_detect(.variable, "hu"), "hu", "b"),
         .variable = str_remove(.variable, "b_(hu_)*"),
         .variable = str_remove_all(.variable, "hoop|condition")) %>%
  separate(.variable, into = c("hoop", "group"), sep = ":") %>%
  mutate(hoop = as_factor(hoop), 
         hoop = fct_relevel(hoop, "near")) -> post

write_csv(post, "1b_post.csv")

dat %>% group_by(person, hoop, condition) %>%
  summarise(Prc = mean(standing_position == 0)) -> df_prc


post %>% filter(param == "hu") %>%
  mutate(p = plogis(.value)) %>%
  rename(hu = ".value") %>%
  select(-param) -> post_hu

ggplot(post_hu, aes(hoop, p, colour = group)) + 
  stat_interval(alpha = 0.5, position = position_dodge(width = 0.5)) +
  scale_y_continuous("Pr(stand at central position)", 
                     limits = c(0, 1), expand = c(0,0)) +
  scale_colour_ptol() +
  scale_fill_ptol()  -> plt_hu

post %>% filter(param == "b") %>%
  mutate(pos = exp(.value)) %>%
  rename(b = ".value") %>%
  select(-param) -> post_b


ggplot(post_b, aes(hoop, pos, colour = group)) + 
  stat_interval(alpha = 0.5, position = position_dodge(width = 0.5)) +
  scale_y_continuous("normalised distance from centre", limits = c(0, 5.2), expand = c(0, 0)) +
  scale_colour_ptol() +
  scale_fill_ptol() -> plt_b

plt_hu + plt_b + plot_layout(guides = "collect")


ggsave("plots/model_fit.png", width = 8, height = 2.6)




















#####################################
# now look at accuracy

dat <- read_sav("DATACOLLECTIONPHYSICAL.sav") 

(dat %>%
	mutate(person = as.factor(1:nrow(dat))) %>%
	select(person, Condition, 
		AccuracyCHoopTrial1, AccuracyCHoopTrial2, AccuracyCHoopTrial3,
		AccuracyMHoopTrial1, AccuracyMHoopTrial2, AccuracyMHoopTrial3,
		AccuracyFHoopTrial1, AccuracyFHoopTrial2, AccuracyFHoopTrial3
		) %>%
	rename(condition = "Condition") %>%
	gather("hoop", "accuracy", -condition, -person) %>%
	mutate(
		condition = as_factor(condition),
		hoop = as_factor(hoop),
		hoop = fct_collapse(hoop, 
			close = c("AccuracyCHoopTrial1", "AccuracyCHoopTrial2", "AccuracyCHoopTrial3"),
			med   = c("AccuracyMHoopTrial1", "AccuracyMHoopTrial2", "AccuracyMHoopTrial3"),
			far   = c("AccuracyFHoopTrial1", "AccuracyFHoopTrial2", "AccuracyFHoopTrial3"))
		) %>%
	group_by(person, condition) %>%
	summarise(accuracy = mean(accuracy))) -> dat 

levels(dat$condition) <- c("Reaching Task", "Logic Puzzles", "Maths Questions")


plt <- ggplot(dat, aes(x = condition, y = accuracy, fill = condition))
plt <- plt + geom_boxplot()
plt <- plt + scale_colour_manual(values = c("#CC6677", "#4477AA", "#117733"))
plt <- plt + theme_bw()
plt <- plt + scale_x_discrete("condition")
plt <- plt + theme(legend.position = "none")
plt <- plt + scale_y_continuous("Accuracy", limits = c(0, 1), expand = c(0,0))
ggsave("accuracy.png", width = 4, height = 3)


