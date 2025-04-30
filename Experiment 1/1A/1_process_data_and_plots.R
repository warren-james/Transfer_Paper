#### This script is for double blind experiment ####
# has data for the reaching and throwing task for each participant
# No need to worry about switch points for this data
# They either did nothing or the reaching task before the throwing task 
#### Notes ####
# for Order: 0 = unprimed. 1 = primed 
# Position is centred on 0 

#### Packages ####
library(tidyverse)
library(ggthemes)
library(brms)
library(tidybayes)
library(patchwork)

options(mc.cores = 8, 
        digits = 2)

theme_set(theme_bw())
#### Constants ####
slab_size <- 0.46

#### load in Data ####
df <- read_csv("data/results.csv") %>%
  rename(Participant.pos = `Participant pos`,
         Hoop.dist = `Hoop dist`,
         Hit = `hit/miss`)

# make participant a factor 
df$Participant <- as_factor(df$Participant)

# sort out levels for Order 
df$Order <- as.factor(df$Order)
levels(df$Order) <- c("Control", "Primed") # , "Optimal"

# Normalise position 
df$Participant.pos <- abs(df$Participant.pos/df$Hoop.dist)

# reorder participants by the lm slope between hoop pos and standing pos
m <- lm(Participant.pos ~ 0 + Participant + Participant:Hoop.dist,
        data = df)

dm <- tibble(Participant = as_factor(1:32), b = m$coefficients[33:64]) %>%
  mutate(Participant = fct_reorder(Participant, b)) %>%
  arrange(b)

df %>% mutate(
  Participant = fct_relevel(Participant,  as.character(dm$Participant))) -> df

#### make plots ####
plt <- ggplot(df, aes(Hoop.dist*slab_size, Participant.pos)) +
  geom_point(data = tibble(x = c(2, 6), y = c(0, 1)),
             aes(x, y), shape  = 4, colour = "black", size = 3 ) + 
  geom_jitter(aes(colour = Order), height = 0, width = 0.1,  alpha = 0.4) + 
  facet_wrap(~Participant, ncol = 8) +
  scale_x_continuous("hoop separation (meters)", limits = c(1,7)) +
  scale_y_continuous("Normalised Standing Position") +
  theme_bw() +
  scale_colour_ptol() +
  theme(strip.background = element_blank(),
        strip.text.x = element_blank(), 
        legend.position = "none") 
plt

# save this 
ggsave("scratch/plots/double_blind.png", width = 8, height = 3.2)

# accuracy plot

df %>% group_by(Participant, Order, Hoop.dist) %>%
  summarise(accuracy = mean(Hit)) %>%
  mutate(Hoop.dist = slab_size * Hoop.dist,
         Hoop.dist= factor(Hoop.dist)) %>%
  rename(group = "Order") %>%
  ggplot(aes(Hoop.dist, accuracy, fill = group)) + 
  geom_boxplot() +
  scale_fill_ptol() +
  theme(strip.background = element_blank()) + 
  scale_y_continuous(limits = c(0, 1), expand = c(0, 0)) +
  scale_x_discrete("hoop separation (meters)", labels = c(2,4,6))

ggsave("scratch/plots/exp1a_acc.png", width = 4, height = 3.2)
#### Analyses ####

# model how relative error varies with Delta (near v far) and group

df %>% filter(Hoop.dist %in% c(5, 13)) %>%
  mutate(hoops = if_else(Hoop.dist == 5, "near", "far"),
         hoops = as_factor(hoops),
         err = if_else(hoops == "near", Participant.pos, 1-Participant.pos),
         err = abs(err)) -> df

my_priors <- c(prior(normal(0, 1), class = sd),
               prior(normal(-1, 2), class = b),
               prior(normal(0, 1), class = sd, dpar = "hu"),
               prior(normal(0, 2), class = b, dpar = "hu"))

# prior model
m <- brm(bf(Participant.pos ~ 0 + hoops + (0 + hoops | Participant),
            hu ~ 0 + hoops + (0 + hoops | Participant)), 
         data = df,
         family = hurdle_lognormal(),
         prior = my_priors,
         iter = 5000,
         backend = "cmdstanr",
         sample_prior = "only")

# get prior predictions
m %>% gather_draws(`[b|hu]_.*`, regex = TRUE) %>%
  mutate(param = if_else(str_detect(.variable, "hu"), "hu", "b"),
         .variable = str_remove(.variable, "b_(hu_)*"),
         .variable = str_remove_all(.variable, "hoops|Order")) %>%
  mutate(hoops = as_factor(.variable), 
         hoops = fct_relevel(hoops, "near")) %>%
  ungroup() %>%
  select(-.variable) -> prior


prior %>% filter(param == "hu") %>%
  mutate(group = "prior", 
         p = plogis(.value)) %>%
  rename(hu = ".value") %>%
  select(-param) -> prior_hu

prior %>% filter(param == "b") %>%
  mutate(group = "prior",
         pos = exp(.value)) %>%
  rename(b = ".value") %>%
  select(-param) -> prior_b


m <- brm(bf(Participant.pos ~ 0 + hoops:Order + (0 + hoops | Participant),
            hu ~ 0 + hoops:Order + (0 + hoops | Participant)), 
         data = df,
         family = hurdle_lognormal(),
         prior = my_priors,
         iter = 5000,
         backend = "cmdstanr")

# plot posterior

df %>% modelr::data_grid(hoops, Order) %>%
  add_predicted_draws(m, re_formula = NA, value = "error") %>%
  ggplot(aes(error, fill = Order)) +
  geom_density(alpha = 0.5) + 
  facet_grid(.~hoops) + 
  ggthemes::scale_color_few() + 
  ggthemes::scale_fill_few() 

m %>% gather_draws(`[b|hu]_.*`, regex = TRUE) %>%
  mutate(param = if_else(str_detect(.variable, "hu"), "hu", "b"),
         .variable = str_remove(.variable, "b_(hu_)*"),
         .variable = str_remove_all(.variable, "hoops|Order")) %>%
  separate(.variable, into = c("hoops", "group"), sep = ":") %>%
  mutate(hoops = as_factor(hoops), 
         hoops = fct_relevel(hoops, "near")) -> post

##########
# first, look at pr(central)
##########

df %>% group_by(Participant, hoops, Order) %>%
  summarise(Prc = mean(Participant.pos == 0)) -> df_prc

post %>% filter(param == "hu") %>%
  mutate(p = plogis(.value)) %>%
  rename(hu = ".value") %>%
  select(-param) %>%
  bind_rows(prior_hu) %>%
  mutate(group = as_factor(group),
         group = fct_relevel(group, "prior")) -> post_hu

ggplot(post_hu, aes(hoops, p, colour = group)) + 
  stat_interval(alpha = 0.5, position = position_dodge(width = 0.5)) +
  scale_y_continuous("Pr(stand at central position)", limits = c(0, 1), expand = c(0, 0)) +
  # geom_jitter(data = df_prc, aes(hoops, Prc, colour = Order), 
  #            shape = 4, height = 0, width = 0.1) + 
  scale_colour_ptol() +
  scale_fill_ptol() -> plt_hu

post %>% filter(param == "b") %>%
  mutate(pos = exp(.value)) %>%
  rename(b = ".value") %>%
  select(-param) %>%
  bind_rows(prior_b) %>%
  mutate(group = as_factor(group),
         group = fct_relevel(group, "prior"))-> post_b

df %>% group_by(Participant, hoops, Order) %>%
  filter(Participant.pos > 0) %>%
  summarise(pos = mean(Participant.pos))-> df_pos

ggplot(post_b, aes(hoops, pos, colour = group)) + 
  stat_interval(alpha = 0.5, position = position_dodge(width = 0.5)) +
  scale_y_continuous("normalised distance from centre", limits = c(0, 1), expand = c(0, 0)) +
  # geom_jitter(data = df_pos, aes(hoops, pos, colour = Order),
  #            shape = 4, height = 0, width = 0.1) + 
  scale_colour_ptol() +
  scale_fill_ptol() -> plt_b

plt_hu + plt_b + plot_layout(guides = "collect")
ggsave("scratch/plots/model_fit.png", width = 8, height = 2.5)


# are there differences between conditions?

# first, central standing?
post_hu %>% select(-hu) %>% 
  pivot_wider(names_from = "group", values_from = "p") %>%
  mutate(Difference = Primed - Control) %>%
  pivot_longer(c("Control", "Primed", "Difference"), names_to = "group", values_to = "hu") -> post_hu_d

# post_hu_d %>%
#   ggplot(aes(hu, fill = group)) +
#   geom_density(alpha = 0.33) + 
#   geom_vline(xintercept = 0, linetype = 2) + 
#   facet_grid(.~hoops) +
#   scale_fill_ptol() -> plt_hu_d

post_hu_d %>% group_by(group, hoops) %>%
  median_hdci(hu) %>%
  knitr::kable()

# and now non-central standing
post_b %>% select(-b) %>% 
  pivot_wider(names_from = "group", values_from = "pos") %>%
  mutate(Difference = Primed - Control) %>%
  pivot_longer(c("Control", "Primed", "Difference"), names_to = "group", values_to = "b") -> post_b_d

# post_b_d %>%
#   ggplot(aes(b, fill = group)) +
#   geom_density(alpha = 0.33) + 
#   geom_vline(xintercept = 0, linetype = 2) + 
#   facet_grid(.~hoops) +
#   scale_fill_ptol()-> plt_b_d

post_b_d %>% group_by(group, hoops) %>%
  median_hdci(b) %>%
  knitr::kable()

# (plt_hu + plt_b) / (plt_hu_d + plt_b_d) +  plot_layout(guides = "collect")


################
# model comparison
#################

# fit simpler model

m2 <- brm(bf(Participant.pos ~ 0 + hoops + (0 + hoops:Order | Participant),
            hu ~ 0 + hoops + (0 + hoops:Order | Participant)), 
         data = df,
         family = hurdle_lognormal(),
         prior = my_priors,
         iter = 5000,
         backend = "cmdstanr")

model_weights(m, m2, method = "loo")
model_weights(m, m2, method = "waic")
