#### Non-naive participants ####

#### libraries needed ####
library(tidyverse)
library(R.matlab)

# load in the data set 
load("scratch/df_part2")

# create a tibble for switch points 
switch_df <- tibble(
  participant = numeric(),
  switch_point = numeric()
)

results_files <- dir("data/switching_points/")

# add in a counter to assign the row
count <- 1

# read in the Matlab data 
for (f in results_files){
  d <- readMat(
    paste("data/switching_points/", f, sep=""))
  
  # get switch point
  d <- d$switchdist[1]

  # get parts of the string for part no.
  temp <- strsplit(f, '[_.]')[[1]]
  
  # now input this information
  switch_df[count,] <- c(temp[2], d)
  
  # increase the count 
  count <- count + 1
  
}

# tidy
rm(count, d, f, temp, results_files)

# combine the datasets
switch_df <- merge(df, switch_df, by="participant")

# tidy
rm(df)


#### Create column for whether they made the optimal choice or not ####
# Make side vs centre first 
# 1 = centre, 2 & 3 = sides
switch_df$centre <- 0
switch_df$centre[switch_df$fixated_box == 1] <- 1


#### save the file (everything) ####
save(switch_df, file = "scratch/switch_data")

#### Create version with Na's removed ####
switch_df <- switch_df[complete.cases(switch_df),]

# create save files
save(switch_df, file = "scratch/switch_nar_data")

rm(switch_df)

