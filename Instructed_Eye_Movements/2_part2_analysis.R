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

make_pred <- function(.draw, group, block, intercept, slope, guess) {
  
  sep = seq(0, 1, 0.01)
  
  eta = intercept + slope * sep
  
  p <- inv_logit(guess) + (1-inv_logit(guess)) * inv_logit(eta)
  
  return(tibble(.draw = .draw, 
                group = group, block = block,
                sep = sep, p = p))

}

m %>% brms::as_draws_df() %>%
  select(-.chain, -.iteration) %>%
  pivot_longer(-.draw, names_to = "param") %>%
  filter(str_detect(param, "b_")) %>%
  mutate(param = str_remove(param, "b_")) %>%
  separate(param, c("eq", "param"), "_") -> post


post %>% 
  filter(eq == "guess") %>%
  select(-eq) %>%
  separate(param, c("group", "block"), ":") %>%
  mutate(group = str_remove(group, "group"),
         block = str_remove(block, "blockblk")) %>%
  rename(guess = "value") -> post_guess

post %>% 
  filter(eq == "eta") %>%
  select(-eq) %>%
  mutate(var = if_else(str_detect(param, ":sep"), "slope", "intercept"),
         param = str_remove_all(param, ":sep")) %>%
  separate(param, c("group", "block"), ":") %>%
  mutate(group = str_remove(group, "group"),
         block = str_remove(block, "blockblk")) %>%
  pivot_wider(names_from = "var") -> post_eta 


post <- full_join(post_eta, post_guess,
                  by = join_by(.draw, group, block))

rm(post_eta, post_guess)


post %>% pmap_df(make_pred) -> pred


pred %>% group_by(group, block, sep) %>%
  median_hdci(p) %>%
  ggplot(aes(sep, p, fill = group)) + 
  geom_ribbon(aes(ymin = .lower, ymax = .upper),
              alpha = 0.5) +
  geom_path(data = adat, 
            aes(y = correct, colour = group, group = interaction(group, participant)),
            alpha = 0.25) + 
  geom_point(data = adat, 
            aes(y = correct, colour = group, group = interaction(group, participant)),
            alpha = 0.25) + 
  geom_hline(yintercept = c(0.75, 0.5), linetype = 2) + 
  facet_grid(. ~ block)


ggplot(post, aes(slope, fill = group)) + geom_density() +
  facet_grid(block ~ .)




####


dat %>% 
  filter(group == c("instruction", "no instruction")) %>% 
  modelr::data_grid(group, block, sep = seq(0, 1, 0.05)) %>% 
  add_epred_draws(m, re_formula = NA) -> pdat

pdat %>% 
  group_by(group, block, sep) %>%
  median_hdci() %>%
  ggplot(aes(sep, .epred, fill = group)) + 
  geom_ribbon(aes(ymin = .lower, ymax = .upper),
              alpha = 0.5) +
  geom_path(data = adat, 
            aes(y = correct, colour = group, 
                group = interaction(group, participant)),
            alpha = 0.1) + 
  geom_hline(yintercept = c(0.75, 0.5), linetype =2) + 
  facet_wrap( ~block) +
  ggthemes::scale_fill_calc() +
  ggthemes::scale_colour_calc() +
  theme_bw()
###


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

