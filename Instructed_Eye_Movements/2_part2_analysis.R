library(tidyverse)
library(tidybayes)
library(brms)

options(mc.cores = 8, 
        digits = 2)



load(file = "m_brms")


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
  filter(.draw < 101) %>%
  ggplot(aes(separation, p, colour = group, group = interaction(group, .draw))) + 
  geom_path(alpha = 0.25) + 
  facet_wrap(~block) +
  theme_bw()
