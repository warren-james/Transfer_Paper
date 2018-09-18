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
