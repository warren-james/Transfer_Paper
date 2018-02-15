#### Script to extract Data ####
# Level 4 Thesis by Elle
# 2017/18
# Script written by Warren James
# This script reads in the data and produces a file containing 
# all participants data

#### libraries needed ####
library(tidyverse)

#### load in the dataset #### 
# create a tibble for dataset 
df <- tibble(
  block = numeric(),
  separation = numeric(), 
  fixated_box = numeric(), 
  correct = numeric())
 
# create column names for the data we have
import_names <- c(
  "block",
  "separation",
  "fixated_box",
  "correct")

# set up directory for loop 
results_files <- dir("data/results/Part_2-3/")

# read in each data file
for (f in results_files){
  d <- read.csv(
    paste("data/results/Part_2-3/", f, sep=""), header = F)
  
  # change column names
  names(d) <- import_names
  
  # get parts of the string for part no. and session number
  temp <- strsplit(f, '[_]')[[1]]
  
  # now input this information
  d$participant <- temp[2]
  
  d$part <- substring(temp[4],5,5)
  
  # bind to df
  df <- bind_rows(df, d)
}

# tidy
rm(temp, f, d, import_names, results_files)

# re-order the dataset 
df <- select(df,
             participant,
             part,
             block,
             separation,
             fixated_box,
             correct)

# save processed data file
save(df, file = "scratch/Elle_data")

# tidy
rm(df)
