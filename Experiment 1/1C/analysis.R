#### Anca study: Optimal Accuracy script ####
# Getting optimal accuracy vs. actual 
# Also standing position 
# making these one plot together

#### libraries ####
library(tidyverse)
library(gridExtra)
library(brms)

options(mc.cores = 8)

theme_set(theme_bw())


#### Any constants ####
slab_size <- 0.46

#### LOAD ####
#### LOAD: part 1 #### 
df_P1 <- read.table("data/part_1/allDataAcc.txt", header = T)

# make participant a factor 
df_P1$Participant <- as.factor(df_P1$Participant)

# collapse across directions and session
df_P1 <- df_P1 %>%
  group_by(Participant, Distance) %>%
  summarise(Accuracy = sum(Accuracy))

# sort out accuracy measure 
df_P1$trials <- 24
df_P1$Acc <- df_P1$Accuracy/df_P1$trials

# do we need the offset?
df_P1$off_set <- log((1-0.01)/0.01)

#### LOAD: Part 2 ####
load("scratch/df_P2")

# get distance from each hoop 
df_P2$South_dist <- abs(df_P2$hoop_dist - df_P2$Position) 
df_P2$North_dist <- abs(df_P2$hoop_dist + df_P2$Position)

#### Get Accuracy accross distances ####
# setup the glm 
acc_sep <- tibble(Participant = character(),
                  Distance = numeric(),
                  pred_Acc = numeric())

# setup vector of slabs 
separations <- c(0:50)

# run GLM for switch points
for(p in levels(df_P1$Participant)){
  # subset 
  ss = df_P1[df_P1$Participant == p,]
  
  # run glm
  m = glm(data = ss, Acc~Distance,
          family = binomial)
  
  y = predict(m, data.frame(Distance = separations), type = "response")
  # y = as.numeric(p)
  
  # bind this to acc_sep
  acc_sep <- rbind(acc_sep, data.frame(Participant = p, 
                                       Distance = c(0:50), 
                                       pred_acc = y))
}

# tidy 
rm(m, ss, p, separations, y)

#### Sort out estimates ####
# now sort out distances part
north_acc <- acc_sep
colnames(north_acc) <- c("Participant",
                         "North_dist",
                         "pred_north")

south_acc <- acc_sep
colnames(south_acc) <- c("Participant",
                         "South_dist",
                         "pred_south")

# merge separately
df_P2 <- merge(df_P2, north_acc)
df_P2 <- merge(df_P2, south_acc)

# tidy 
rm(north_acc, south_acc)

df_P2$Exp_acc <- (df_P2$pred_north + df_P2$pred_south)/2

# remove unused columns 
df_P2 <- df_P2 %>%
  select(-c(pred_north,
            pred_south,
            Colour,
            Direction,
            South_dist,
            North_dist))

# now sort out Optimal standing pos 
df_P2$opt_pos <- 0 
df_P2$opt_pos[df_P2$hoop_dist > df_P2$switch_point] <- 1

df_P2$Opt_acc <- df_P2$Exp_acc
df_P2$Opt_acc[df_P2$opt_pos == 1] <- 0.5

# again, reduce data set 
df_P2 <- df_P2 %>%
  select(-opt_pos) %>%
  as_tibble()


################ model

my_priors <- c(prior(normal(0, 0.5), class = sd),
               prior(normal(-1.5, 1), class = b),
               prior(normal(0, 0.5), class = sd, dpar = "hu"),
               prior(normal(0, 1.5), class = b, dpar = "hu"))


dat <- df_P2 %>% filter(hoop_dist %in% c(5, 17), Condition != "tableFail") %>%
  mutate(hoop = as_factor(hoop_dist),
         hoop = fct_recode(hoop, near = "5", far = "17")) %>%
  rename(person = "Participant", condition = "Condition", standing_position = "Norm_pos")

m <- brm(bf(standing_position ~ 0 + hoop:condition + (0 + hoop:condition | person),
            hu ~ 0 + hoop:condition + (0 + hoop:condition | person)), 
         data = dat,
         family = hurdle_lognormal(),
         prior = my_priors,
         iter = 5000,
         backend = "cmdstanr")


# plot posterior
m %>% gather_draws(`[b|hu]_.*`, regex = TRUE) %>%
  mutate(param = if_else(str_detect(.variable, "hu"), "hu", "b"),
         .variable = str_remove(.variable, "b_(hu_)*"),
         .variable = str_remove_all(.variable, "hoop|condition")) %>%
  separate(.variable, into = c("hoop", "group"), sep = ":") %>%
  mutate(hoop = as_factor(hoop), 
         hoop = fct_relevel(hoop, "near")) -> post

write_csv(post, "1c_post.csv")
# 
# dat %>% group_by(person, hoop, condition) %>%
#   summarise(Prc = mean(standing_position == 0)) -> df_prc
# 
# 
# post %>% filter(param == "hu") %>%
#   mutate(p = plogis(.value)) %>%
#   rename(hu = ".value") %>%
#   select(-param) -> post_hu
# 
# ggplot(post_hu, aes(hoop, p, colour = group)) + 
#   stat_interval(alpha = 0.5, position = position_dodge(width = 0.5)) +
#   scale_y_continuous("Pr(stand at central position)", 
#                      limits = c(0, 1), expand = c(0,0)) +
#   scale_colour_ptol() +
#   scale_fill_ptol()  -> plt_hu
# 
# post %>% filter(param == "b") %>%
#   mutate(pos = exp(.value)) %>%
#   rename(b = ".value") %>%
#   select(-param) -> post_b
# 
# 
# ggplot(post_b, aes(hoop, pos, colour = group)) + 
#   stat_interval(alpha = 0.5, position = position_dodge(width = 0.5)) +
#   scale_y_continuous("normalised distance from centre", limits = c(0, 5.2), expand = c(0, 0)) +
#   scale_colour_ptol() +
#   scale_fill_ptol() -> plt_b
# 
# plt_hu + plt_b + plot_layout(guides = "collect")
# 
# 
# ggsave("plots/model_fit.png", width = 8, height = 2.6)
# 
