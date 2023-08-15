library(tidyverse)
library(haven)
library(ggthemes)
library(brms)
library(patchwork)
library(tidybayes)


theme_set(theme_bw())



prior <- read_csv("1B/1_prior.csv")

post_b <- read_csv("1B/1b_post.csv")
post_c <- read_csv("1C/1c_post.csv")

post_b %>% mutate(group = as_factor(group),
                  group = fct_recode(group, 
                                     "1b: reaching" = "ReachingTask",
                                     "1b: logic" = "LogicPuzzles",
                                     "1b: maths" = "MathsQuestions")) -> post_b


post_c %>% mutate(group = as_factor(group),
                  group = fct_recode(group, 
                                     "1c: reaching" = "table",
                                     "1c: sudoku" = "sudoku")) -> post_c

post <- bind_rows(prior, post_b, post_c) %>%
  rename(hoops = "hoop") %>%
  mutate(group = as_factor(group),
         hoops = as_factor(hoops), 
         hoops = fct_relevel(hoops, "near"))

                  

post %>% filter(param == "hu") %>%
  mutate(p = plogis(.value)) %>%
  rename(hu = ".value") %>%
  select(-param) -> post_hu

ggplot(post_hu, aes(hoops, p, colour = group)) + 
  stat_interval(alpha = 0.5, position = position_dodge(width = 0.5)) +
  scale_y_continuous("Pr(stand at central position)", 
                     limits = c(0, 1), expand = c(0,0)) +
  scale_colour_manual(values = c("#aaaaaa", "#0077bb", "#33bbee", "#009988", "#ee7733", "#cc3311"))  -> plt_hu

post %>% filter(param == "b") %>%
  mutate(pos = exp(.value)) %>%
  rename(b = ".value") %>%
  select(-param) -> post_b


ggplot(post_b, aes(hoops, pos, colour = group)) + 
  stat_interval(alpha = 0.5, position = position_dodge(width = 0.5)) +
  scale_y_continuous("normalised distance from centre", limits = c(0, 5.2), expand = c(0, 0)) +
  scale_colour_manual(values = c("#aaaaaa", "#0077bb", "#33bbee", "#009988", "#ee7733", "#cc3311")) +
  geom_hline(yintercept = 1, linetype  =2)-> plt_b

plt_hu + plt_b + plot_layout(guides = "collect")
ggsave("exp1bc_post.png", width = 8, height = 3)
