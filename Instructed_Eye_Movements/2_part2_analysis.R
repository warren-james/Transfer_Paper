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
  modelr::data_grid(group = c("instruction", "no instruction"), block, sep = seq(0, 1, 0.1)) %>% 
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


m %>% gather_draws(`b_.*`, regex = TRUE) %>%
  select(-.chain, -.iteration) %>%
  mutate(param = if_else(str_detect(.variable, "sep"), "slope", "intercept"),
         .variable = str_remove(.variable, "b_(hu_)*"),
         .variable = str_remove(.variable, ":sep")) %>%
  pivot_wider(names_from = "param", values_from = ".value") %>%
  separate(.variable, into = c("group", "block"), sep = ":") %>%
  mutate(group = str_remove(group, "group"),
         block = str_remove(block, "block"),
         block = factor(block, labels = c("block 1", "block 2"))) -> post


get_acc <- function(.draw, group, block, intercept, slope) {
  
  separation = seq(0, 1, 0.01)
  
  y <- intercept + slope * separation
  
  return(tibble(
    .draw = .draw, group = group, block = block,
    separation = separation,
    p = plogis(y)))
  
}

pred <- pmap_df(post, get_acc)


pred %>% 
  filter(.draw < 201) %>%
  ggplot(aes(separation, p, colour = group, group = interaction(group, .draw))) + 
  geom_path(alpha = 0.15) + 
  facet_wrap(~block) +
  theme_bw()

pred %>% 
  group_by(group, block, separation) %>%
  median_hdci(p) %>%
  ggplot(aes(separation, ymin = .lower, ymax = .upper, fill = group)) + 
  geom_ribbon(alpha = 0.25) +
  facet_wrap(~block) + 
  scale_fill_viridis_d()
