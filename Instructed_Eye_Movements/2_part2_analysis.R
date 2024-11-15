library(tidyverse)
library(tidybayes)
library(brms)

options(mc.cores = 8, 
        digits = 2)


load(file = "m_brms")

inv_logit <- function(x) {
  z <- plogis(x)
  return(z)
}


dat %>% 
  modelr::data_grid(group = c('instruction', 'no instruction'), block, sep = seq(-5, 5, 0.1))  %>% 
  add_epred_draws(m, re_formula = NA) -> pdat

dat %>%
  filter(is.finite(correct), group != "simulated") %>%
  group_by(participant, group, block, sep) %>%
  summarise(correct = mean(correct)) -> adat

pdat %>% 
  group_by(group, block, sep) %>%
  median_hdci() %>%
  ggplot(aes(sep, .epred, fill = group)) + 
  geom_ribbon(aes(ymin = .lower, ymax = .upper),
              alpha = 0.5) +
  geom_path(data = adat, 
            aes(y = correct, colour = group, group = interaction(group, participant)),
            alpha = 0.25) + 
  geom_point(data = adat, 
            aes(y = correct, colour = group, group = interaction(group, participant)),
            alpha = 0.25) + 
  geom_hline(yintercept = c(0.75, 0.5), linetype =2) + 
  facet_grid(. ~ block)



####

bind_rows(
  dat %>% 
    filter(group == "instruction") %>% 
    modelr::data_grid(participant, group, block, sep = seq(0, 1, 0.1)),
  dat %>% 
    filter(group == "no instruction") %>% 
    modelr::data_grid(participant, group, block, sep = seq(0, 1, 0.1))) %>% 
  add_epred_draws(m, re_formula = NULL) -> pdat


pdat %>% 
  group_by(participant, group, block, sep) %>%
  median_hdci() %>%
  ggplot(aes(sep, .epred, fill = group)) + 
  geom_ribbon(aes(ymin = .lower, ymax = .upper),
              alpha = 0.5) +
  geom_path(data = adat, 
          aes(y = correct), colour = "black",
          alpha = 0.25) + 
  geom_hline(yintercept = c(0.75, 0.5), linetype =2) + 
  facet_grid(block ~ participant)
###

