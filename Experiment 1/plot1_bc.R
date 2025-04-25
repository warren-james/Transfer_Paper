library(tidyverse)
library(haven)
library(ggthemes)
library(brms)
library(patchwork)
library(tidybayes)


theme_set(theme_bw())

#### Any constants ####
slab_size <- 0.46

# plot accuracy

db <- read_sav("1B/data/DATACOLLECTIONPHYSICAL.sav") 
load("1C/scratch/df_P2")
dc <- as_tibble(df_P2)
rm(df_P2)

# process 1b
db %>%
    mutate(person = (1:nrow(db))) %>%
    select(person, Condition, 
           AccuracyCHoopTrial1, AccuracyCHoopTrial2, AccuracyCHoopTrial3,
           AccuracyMHoopTrial1, AccuracyMHoopTrial2, AccuracyMHoopTrial3,
           AccuracyFHoopTrial1, AccuracyFHoopTrial2, AccuracyFHoopTrial3) %>%
    rename(condition = "Condition") %>%
  pivot_longer(-c(person, condition), names_to = "hoop", values_to = "hit") %>%
  mutate(
      condition = as_factor(condition),
      hoop = as_factor(hoop),
      hoop = fct_collapse(hoop, 
                          near = c("AccuracyCHoopTrial1", "AccuracyCHoopTrial2", "AccuracyCHoopTrial3"),
                          med   = c("AccuracyMHoopTrial1", "AccuracyMHoopTrial2", "AccuracyMHoopTrial3"),
                          far   = c("AccuracyFHoopTrial1", "AccuracyFHoopTrial2", "AccuracyFHoopTrial3"))) %>%
  group_by(person, condition, hoop) %>%
  summarise(accuracy = mean(hit)) %>%
  mutate(condition = fct_recode(condition, 
                            "1b: reaching" = "ReachingTask",
                            "1b: logic" = "LogicPuzzle",
                            "1b: maths" = "MathsQuestions")) -> db

# process 1c
dc %>% group_by(Participant, hoop_dist, Condition) %>%
  summarise(accuracy = mean(Accuracy)) %>%
  rename(condition = "Condition", hoop = "hoop_dist", person = "Participant") %>%
  mutate(hoop = factor(slab_size * hoop),
         hoop = fct_recode(hoop, near = "2.3", med = "4.14", med = "5.98", far = "7.82"),
         condition = fct_recode(condition, 
                            "1c: reaching" = "table",
                            "1c: sudoku" = "sudoku")) -> dc

dacc <- bind_rows(db , dc) %>%
  filter(condition != "tableFail")

dacc %>% ggplot(aes(hoop, accuracy, colour = condition)) +
  geom_jitter(alpha = 0.5, height = 0, width = 0.1) + 
  scale_colour_manual(values = c("#ee7733", "#33bbee", "#009988", "#0077bb" , "#cc3311"))

#####################################
# plot the Bayesian model
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
  scale_colour_manual(values = c("#aaaaaa", "#ee7733", "#33bbee", "#009988", "#0077bb" , "#cc3311"))  -> plt_hu

post %>% filter(param == "b") %>%
  mutate(pos = exp(.value)) %>%
  rename(b = ".value") %>%
  select(-param) -> post_b


ggplot(post_b, aes(hoops, pos, colour = group)) + 
  stat_interval(alpha = 0.5, position = position_dodge(width = 0.5)) +
  scale_y_continuous("normalised distance from centre", limits = c(0, 4), expand = c(0, 0)) +
  scale_colour_manual(values = c("#aaaaaa", "#ee7733", "#33bbee", "#009988", "#0077bb" , "#cc3311")) +
  geom_hline(yintercept = 1, linetype  =2) +
  coord_cartesian(ylim = c(0, 2)) -> plt_b

plt_hu + plt_b + plot_layout(guides = "collect")
ggsave("exp1bc_post.png", width = 8, height = 2.5)


post %>% 
  mutate(.value = if_else(param=="hu", plogis(.value), exp(.value))) -> post

post %>%
  group_by(param, hoops, group) %>%
  median_hdci(.value) %>%
  knitr::kable()

# compute differences
post %>% filter(hoops == "far", str_detect(group, "1b")) %>%
  pivot_wider(names_from = group, values_from = ".value") %>%
  mutate(reach_diff = `1b: maths` - `1b: reaching`,
         logic_diff = `1b: maths` - `1b: logic`) %>%
  select(param, reach_diff, logic_diff) %>%
  group_by(param) %>%
  median_hdci(reach_diff,logic_diff)


post %>% filter(hoops == "far", str_detect(group, "1c")) %>%
  pivot_wider(names_from = group, values_from = ".value") %>%
  mutate(reach_diff = `1c: sudoku` - `1c: reaching`) %>%
  select(param, reach_diff) %>%
  group_by(param) %>%
  median_hdci(reach_diff)
