##### Make dataset script #####
# Use this to make kind of straight line 

#### libraries ####
library(tidyverse)
library(rstan)

#### make data ####
x <- seq(1,100,1)
y <- x + runif(100, min = 0, max = 20)



