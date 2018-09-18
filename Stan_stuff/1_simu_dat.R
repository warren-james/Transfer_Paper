##### Make dataset script #####
# Use this to make kind of straight line 

#### libraries ####
library(tidyverse)
library(rstan)

#### make data ####
x <- seq(1,100,1)
y <- x + runif(100, min = 0, max = 20)

#### fit distribution ####
# create a list of the data 
sim_data <- list(
  N = length(y),
  y = y
)

# create model fit
fit1 <- stan(
  file = "fit_a_gaussian.stan",
  data = sim_data,
  chains = 4,
  warmup = 1000,
  iter = 2000,
  cores = 2,
  refresh = 100
)

#### linear regression ####
lin_data <- list(
  N = length(y),
  y = y, 
  x = x
)

# create model fit
fit2 <- stan(
  file = "linear_regression.stan",
  data = lin_data,
  chains = 4,
  warmup = 1000,
  iter = 2000,
  cores = 2,
  refresh = 100
)


#### GLM ####
# on binary data
# setup data 
x = seq(-5, 5, 0.1)
y = 1/(1+exp(-(x + 2 * runif(length(x))))) > 0.5
  
# setup list 
glm_dat <- list(
  x = x,
  y = y,
  N = length(y)
)

# fit model 
# this is bad because we're fitting a linear regression
# to binary data
fit3 <- stan(
  file = "linear_regression.stan", 
  data = glm_dat,
  chains = 1,
  warmup = 1000,
  iter = 2000,
  refresh = 100
)

# extract samples 
post_fit3 <- rstan::extract(fit3)

b <- mean(post_fit3$b)
c <- mean(post_fit3$c)

# good(ish) plot, bad model
plot(x,y)
lines(x, b*x+c)

#### logistic regression ####
# now as a logistic regression
fit4 <- stan(
  file = "logistic_regression.stan", 
  data = glm_dat,
  chains = 1,
  warmup = 1000,
  iter = 2000,
  refresh = 100
)

# extract samples 
post_fit4 <- rstan::extract(fit4)

b <- mean(post_fit4$b)
c <- mean(post_fit4$c)

# good(ish) plot, bad model
plot(x,y)
lines(x, logistic(b*x+c))

#### chance performance ####
