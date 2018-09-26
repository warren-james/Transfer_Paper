  #### Alasdair's Analysis
#### libraries needed ####
library(tidyverse)
library(rethinking)
# library(viridisLite)
library(ggthemes)
# rethinking options 
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())



# # load in the data set 
load("scratch/Elle_switch_nar_data")
df <- switch_df
rm(switch_df)

#  tidy up
df$switch_point <- as.numeric(df$switch_point)


df$given_instruction <- as.numeric(df$condition == "Instructions")

df$sep_scaled <- df$separation / max(df$separation)

#### now do all sessions together ####
df$second_half <- as.numeric(df$block > 4)

# save this 
save(df, file = "scratch/models_df")

# not taking separation in to account
m1 <- map2stan( 
  alist(
    correct ~ dbinom(1, p),
    logit(p) <- a + a_p[participant] +       
      instruction * given_instruction + 
      half * second_half + 
      halfbyinst * second_half * given_instruction,
    a ~ dnorm(0, 1),   
    instruction ~ dnorm(0, 1),
    half ~ dnorm(0, 1),
    halfbyinst ~ dnorm(0, 1),
    a_p[participant] ~ dnorm(0, sigma_p),
    sigma_p ~ dcauchy(0, 1)),
  data = df,
  iter = 4000, warmup = 1000, chains = 4, cores = 4)

# save
save(m1, file = "scratch/models/A_models/m1")

# taking separation in to account, no interactions
m2 <- map2stan( 
  alist(
    correct ~ dbinom(1, p),
    logit(p) <- 
      a + a_p[participant] + 
      b * sep_scaled + 
      instruction * given_instruction + 
      half * second_half + 
      halfbyinst * second_half * given_instruction,
    a ~ dnorm(0, 1),
    b ~ dnorm(0, 1),
    instruction ~ dnorm(0, 1),
    half ~ dnorm(0, 1),
    halfbyinst ~ dnorm(0, 1),
    a_p[participant] ~ dnorm(0, sigma_p),
    sigma_p ~ dcauchy(0, 1)),
  data = df,
  iter = 4000, warmup = 1000, chains = 4, cores = 4)

# save
save(m2, file = "scratch/models/A_models/m2")

# taking separation in to account, including interaction with practice effect
m3 <- map2stan( 
  alist(
    correct ~ dbinom(1, p),
    logit(p) <- 
      a + a_p[participant] + 
      b * sep_scaled + 
      instruction * given_instruction + 
      half * second_half + 
      halfbysep * second_half * sep_scaled + 
      halfbyinst * second_half * given_instruction,
    a ~ dnorm(0, 1),
    b ~ dnorm(0, 1),
    instruction ~ dnorm(0, 1),
    half ~ dnorm(0, 1),
    halfbysep ~ dnorm(0, 1),
    halfbyinst ~ dnorm(0, 1),
    a_p[participant] ~ dnorm(0, sigma_p),
    sigma_p ~ dcauchy(0, 1)),
  data = df,
  iter = 4000, warmup = 1000, chains = 4, cores = 4)

# save
save(m3, file = "scratch/models/A_models/m3")

# taking separation in to account, more interactions
m4 <- map2stan( 
  alist(
    correct ~ dbinom(1, p),
    logit(p) <- 
      a + a_p[participant] + 
      b * sep_scaled + 
      instruction * given_instruction + 
      instrbysep * given_instruction * sep_scaled + 
      half * second_half + 
      halfbysep * second_half * sep_scaled + 
      halfbyinst * second_half * given_instruction,
    a ~ dnorm(0, 1),
    b ~ dnorm(0, 1),
    instruction ~ dnorm(0, 1),
    instrbysep  ~ dnorm(0, 1),
    half ~ dnorm(0, 1),
    halfbysep ~ dnorm(0, 1),
    halfbyinst ~ dnorm(0, 1),
    a_p[participant] ~ dnorm(0, sigma_p),
    sigma_p ~ dcauchy(0, 1)),
  data = df,
  iter = 4000, warmup = 1000, chains = 4, cores = 4)

# save
save(m4, file = "scratch/models/A_models/m4")


# taking separation in to account, more interactions
m5 <- map2stan( 
  alist(
    correct ~ dbinom(1, p),
    logit(p) <- 
      a + a_p[participant] + 
      b * sep_scaled + 
      instruction * given_instruction + 
      instrbysep * given_instruction * sep_scaled + 
      half * second_half + 
      halfbysep * second_half * sep_scaled + 
      halfbyinst * second_half * given_instruction + 
      threeway * second_half * given_instruction * sep_scaled,
    a ~ dnorm(0, 1),
    b ~ dnorm(0, 1),
    instruction ~ dnorm(0, 1),
    instrbysep  ~ dnorm(0, 1),
    half ~ dnorm(0, 1),
    halfbysep ~ dnorm(0, 1),
    halfbyinst ~ dnorm(0, 1),
    threeway ~ dnorm(0, 1),
    a_p[participant] ~ dnorm(0, sigma_p),
    sigma_p ~ dcauchy(0, 1)),
  data = df,
  iter = 4000, warmup = 1000, chains = 4, cores = 4)


# save this model 
save(m1, m2, m3, m4, m5, file = "scratch/models/models")

load("scratch/models/models")

compare(m1, m2, m3, m4)


# plot based on m4, as it appears to be the best
post <- extract.samples(m4)

# post$ etc saved as a,b,c,d for now


get_fx_for_sep <- function(d, post) {
  
  fx <- tibble(
    condition = rep(c(
      "baseline", 
      "instructed", 
      "practice",
      "transfer"), each = length(post$a)),
    delta = d,
    samples = c(
      logistic(post$a + post$b * d), 
      logistic(post$a + post$instruction + post$b  * d + post$instrbysep * d), 
      logistic(post$a + post$half + post$b  * d + post$halfbysep  * d),
      logistic(post$a + post$instruction + post$half + post$halfbyinst + post$b * d + post$instrbysep * d + post$halfbysep * d)))

    return(fx)
}


fx_min_sep  <- get_fx_for_sep(min(df$sep_scaled), post)
fx_mean_sep <- get_fx_for_sep(mean(df$sep_scaled), post)
fx_max_sep  <- get_fx_for_sep(max(df$sep_scaled), post)

fx <- bind_rows(fx_min_sep, fx_mean_sep, fx_max_sep)

# turn delta in to a labelled factor for plotting
fx$delta <- as.factor(fx$delta)
levels(fx$delta) = c("close", "medium", "far")

plt <- ggplot(fx, aes(x = samples, fill = condition))
plt <- plt + geom_density(alpha = 0.6)
plt <- plt + theme_bw()  
plt <- plt + scale_fill_manual(values = c("#228833", "#CCBB44", "#4477AA", "#CC6677"))
plt <- plt + facet_wrap(~delta)
plt <- plt + scale_x_continuous(
  name = "probablity of correctly responding to target", 
  limits = c(0.4,1), expand = c(0,0),
  breaks = c(0.5, 0.75, 1))
plt <- plt + scale_y_continuous(expand = c(0, 0), limits = c(0, 40))
ggsave("scratch/plots/joint_model.png", width = 8, height = 3)
plt

#  calculate HDPI

# effect of practice
print(HPDI(logistic(post$a + post$half) - logistic(post$a), prob = 0.95))

# effect of instructions
print(HPDI(logistic(post$a + post$instruction) - logistic(post$a), prob = 0.95))

# transfer effect (compared to control group)
print(HPDI(logistic(post$a + post$b + post$half + post$instruction) - logistic(post$a + post$half), prob = 0.95))







