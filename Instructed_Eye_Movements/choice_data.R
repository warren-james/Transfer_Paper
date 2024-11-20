# new script to analyse choice data



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

# fix sep variable
dat$sep <- scale(dat$sep)

# try making block more categorical in its labels
dat %>% mutate(block = if_else(block == "1", "block 1", "block 2")) -> dat


dat %>% group_by(participant, group, block, sep) %>%
  summarise(fixated_side = mean(fixated_side)) -> adat

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
     fixated_side ~ 0 + group:block + group:block:sep + (0 + group:block + group:block:sep | participant),
     family = bernoulli(),
     prior = my_priors,
     chains = 4,
     iter = 2000)

save(m, file = "mchoice_brms")

dat %>% 
  modelr::data_grid(group, block, sep = seq(-2, 2.5, 0.05)) %>% 
  add_epred_draws(m, re_formula = NA) %>% 
  group_by(group, block, sep) %>%
  median_hdci(.width = c(0.5, 0.95)) %>%
  ggplot(aes(sep, .epred, fill = group)) + 
  geom_ribbon(aes(ymin = .lower, ymax = .upper, 
                  group = interaction(group, .width)),
              alpha = 0.5) +
  # geom_path(data = adat, 
  #           aes(y = fixated_side, colour = group, 
  #               group = interaction(group, participant)),
  #           alpha = 0.10) + 
  facet_wrap( ~block) +
  ggthemes::scale_fill_ptol() +
  ggthemes::scale_colour_ptol() +
  theme_bw()


