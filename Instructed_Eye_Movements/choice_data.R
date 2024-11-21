# new script to analyse choice data
library(tidyverse)
library(brms)
library(tidybayes)
library(patchwork)

options(mc.cores = 8, digits = 2)


folder <- "data/results/Part_2-3/"
files <- dir(folder, full.names = TRUE)

# parse condition info, saved as part = 2 or 3 in filename
condition_code <- regmatches(files, regexpr('(?<=part)[23]*', files, perl = TRUE))
condition_code <- if_else(condition_code == "2", "no instruction", "instruction")

# parse participant info
p_id <- regmatches(files, regexpr('(?<=_)[0-9]*(?=_)', files, perl = TRUE))

# read in data
map_df(files, read_csv,
       col_names = c("block", "sep", "fixated", "correct"),
       show_col_types = FALSE) %>%
  mutate(
    participant = rep(p_id, each = 90),
    group = as_factor(rep(condition_code, each = 90)),
    block = if_else(block < 5, 1, 2)) %>%
  select(participant, group, block, sep, fixated, correct) -> dat

# fix block two labels
intructed_group <- dat %>% filter(block == 1, group == "instruction") %>%
  group_by(participant) %>% summarise()

dat %>% mutate(
  group = if_else((participant %in% intructed_group$participant) & (group != "simulated") & (block == 2),
                  "instruction", group)) -> dat


dat %>% mutate(fixated_side = if_else(fixated != 1, 1, 0)) -> dat

# fix sep variable - convert to visual degrees
# centre on 7 degrees (around where the average switch point is)
dat$sep <- dat$sep/ 35.5 - 7

# try making block more categorical in its labels
dat %>% mutate(block = if_else(block == "1", "block 1", "block 2")) -> dat

dat %>% group_by(participant, group, block, sep) %>%
  summarise(fixated_side = mean(fixated_side)) %>%
  mutate(
    group = factor(group),
    group = fct_relevel(group, "no instruction", "instruction")) -> adat

adat %>%
  ggplot(aes(sep, fixated_side, colour = group, group = participant)) +
  geom_point() + geom_path() +
  facet_wrap(~block)

# priors!
my_priors <- c(
  prior(normal(0, 1), class = b),
  prior(normal(0, 2), class = b, coef = "groupinstruction:blockblock1:sep"),
  prior(normal(0, 2), class = b, coef = "groupnoinstruction:blockblock1:sep"),
  prior(normal(0, 2), class = b, coef = "groupinstruction:blockblock2:sep"),
  prior(normal(0, 2), class = b, coef = "groupnoinstruction:blockblock2:sep"),
  prior(normal(0, 1), class = sd))

m <- brm(data = dat,
     fixated_side ~ 0 + group:block + group:block:sep + (0 + block + block:sep | participant),
     family = bernoulli(),
     prior = my_priors,
     chains = 4,
     iter = 1000)

save(m, file = "mchoice_brms")

dat %>% 
  modelr::data_grid(group, block, sep = seq(-7, 12, 0.1)) %>% 
  add_epred_draws(m, re_formula = NA) %>% 
  group_by(group, block, sep) %>%
  median_hdci(.width =  0.95) %>%
  mutate(group = fct_relevel(group, "no instruction", "instruction"),
         sep = sep + 7) %>%
  ggplot(aes(sep, .epred, fill = group)) +
  geom_path(data = adat %>% mutate(sep = sep + 7),
            aes(y = fixated_side,
                colour = group,
                group = interaction(group, participant)),
            alpha = 0.25) +
  geom_ribbon(aes(ymin = .lower, ymax = .upper, 
                  group = interaction(group, .width)), 
                  alpha = 0.50)  +
  facet_wrap(~block) +
  ggthemes::scale_fill_ptol() +
  ggthemes::scale_colour_ptol() +
  scale_y_continuous("prob. fixate central square") + 
  scale_x_continuous("target separation (visual degrees)") + 
  theme_bw() -> plt1

m %>% as_draws_df(variable = "b_", regex = TRUE) %>%
  as_tibble() %>%
  select(-.iteration, -.chain)%>%
  pivot_longer(-.draw, names_to = "parameter", values_to = "z") %>%
  mutate(slope = if_else(str_detect(parameter, ":sep"), "slope", "intercept"),
         parameter = str_remove(parameter, ":sep"),
         parameter = str_remove(parameter, "b_")) %>%
  pivot_wider(names_from = "slope", values_from = "z") %>%
  separate(parameter, c("group", "block"), sep = ":") %>%
  mutate(group = if_else(str_detect(group, "no"), "no instruction", "instruction"),
         block = if_else(str_detect(block, "1"), "1", "2")) -> post

post %>%
  filter(block == "2") %>%
  select(-intercept) %>%
  pivot_wider(names_from = "group", values_from = "slope") %>%
  mutate(difference = instruction - `no instruction`) %>%
  pivot_longer(-c(.draw, block), names_to = "group", values_to = "slope") %>%
  mutate(group = fct_relevel(group, "no instruction", "instruction")) -> post
  
post %>%
  ggplot(aes(slope, fill = group)) + ggplot(aes(slope, fill = group)) + post
  geom_density(alpha = 0.5) + 
  geom_vline(xintercept = 0, linetype = 2) +
  theme_bw() + 
  scale_fill_manual(values = c("#4477AA", "#CC6677", "#777777")) +
  theme(legend.position = "none") -> plt2




post %>%
  group_by(group) %>%
  median_hdci(slope) %>%
  knitr::kable()


### difference in acc

dat %>% 
  group_by(block, group, participant) %>%
  summarise(accuracy = mean(correct, na.rm = T)) %>%
  mutate(
    group = factor(group),
    group = fct_relevel(group, "no instruction", "instruction"),
    block = str_remove(block, "block ")) -> acc

acc %>% 
  ggplot(aes(block, accuracy, fill = group)) + 
  geom_boxplot(alpha = 0.75) +
  ggthemes::scale_fill_ptol() +
  theme_bw() +
  theme(legend.position = "none") -> plt_acc

plt1 + plt2 + plt_acc + plot_layout(guides = "collect", widths = c(3, 1, 1))
ggsave("exp2_results.png", width = 10, height = 3)
# 
# %>%
#   summarise(
#     q1_acc = quantile(accuracy, 0.25),
#     accuracy = mean(accuracy),
#     q3_acc = quantile(accuracy, 0.75))
