#### Plotting Script #### 
# Level 4 Thesis by Elle
# 2017/18
# Written by Warren James
# Script for processing data to be analysed by Elle
# This script will produce datasets about:
# Expected accuracy had they performed optimally
# Expected accuracy based on fixation locations (to account for chance)
# Proportion of time looking at side vs centre across 7 tested distances
#    - Also by block number (first half vs second half)
# Actual Accuracy accross blocks: 
#    - Doesn't Alasdair's script do this one?
# 6_?

#### libraries needed ####
library(tidyverse)

#### Extra information ####
# I think there were 36 pixels per degree... Will have to check though

#### Read in part 1 measures for accuracy across distances ####
# create dataframe
df <- tibble(
  block = numeric(),
  separation = numeric(),
  accuracy = numeric()
)

# colnames 
import_names <- c(
  "block",
  "separation",
  "accuracy"
)

# set up directory for loop 
results_files <- dir("data/results/Part_1/")

# read in data 
for (f in results_files){
  d <- read.csv(
    paste("data/results/Part_1/", f, sep=""), header = F)
  
  # change column names
  names(d) <- import_names
  
  # get parts of the string for part no. and session number
  temp <- strsplit(f, '[_]')[[1]]
  
  # now input this information
  d$participant <- temp[2]
  
  # bind to df
  df <- bind_rows(df, d)
}

# tidy 
rm(d, f, import_names, results_files, temp)

# re-order
df <- select(df,
             participant,
             block,
             separation,
             accuracy)

# remove NA trials 
df <- df[complete.cases(df),]

#### Calculate curve for accuracy across distances ####
sep <- seq(100,450,10)

# Doesn't work the way I think it should be working...
for (p in unique(df$participant))
{
  ss = df[which(df$participant==p),]
  m = glm(data=ss, accuracy~separation, family=binomial(mafc.probit(2)))
  # y = predict(m, data.frame(separation = unique(df$separation), type = "correct"))
  y = predict(m, data.frame(separation = sep, type = "response"))
  # not sure which is better to use ...
}





