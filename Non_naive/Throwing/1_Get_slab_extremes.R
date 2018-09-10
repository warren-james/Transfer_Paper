#### Script to get slabs to test ####
# This script is to get the average performance for all participants
# that have participated in the throwing experiment using the normal 
# sized hoops so far. 

# Just need the extremes so we can sample within this randomly. 

#### load libraries ####
library(tidyverse)

#### any functions ####

#### read in data ####
results_files <- dir("data/Part1/")

# Need to read in each individually since they're different formats
anca_dat <- read.table(paste("data/Part1/", results_files[1], sep = ""),
                       header = T)

aware_dat <- read.csv(paste("data/Part1/", results_files[2], sep = ""))

CH_dat <- read.csv(paste("data/Part1/", results_files[3], sep = ""))

TwoT_dat <- read.csv(paste("data/Part1/", results_files[4], sep = ""))

# sort colnames
anca_dat <- select(anca_dat, 
                   Participant,
                   Direction,
                   Distance,
                   Accuracy)

colnames(aware_dat) <- c("Participant",
                         "Direction",
                         "Distance",
                         "Accuracy")

colnames(CH_dat) <- c("Participant",
                         "Direction",
                         "Distance",
                         "Accuracy")

colnames(TwoT_dat) <- c("Participant",
                         "Direction",
                         "Distance",
                         "Accuracy")

# before merging, make sure there is no overlap in participant coding
anca_dat$Participant <- paste(anca_dat$Participant, "AN", sep = "")

# bind 
df <- rbind(anca_dat, aware_dat)
df <- rbind(df, CH_dat)
df <- rbind(df, TwoT_dat)

# tidy 
rm(anca_dat, aware_dat, CH_dat, TwoT_dat, results_files)

# factor participants 
df$Participant <- as.factor(df$Participant)

#### Get GLM ####
# need total trials 
df$trials <- 12

# get Acc
df$Acc <- df$Accuracy/df$trials

# offset
e <- 0.01
df$off_set = log((1-e)/e)

# tidy 
rm(e)

# Probably want to extract for each participant
# So we need a loop to get the max and minimum
# set up empty frame 
slabs_to_test = data.frame(Participant=character(),
                           slab10 = numeric(),
                           slab90 = numeric())

for (x in levels(df$Participant)){
  # subset data
  ss = df[df$Participant==x,]
  # get glm 
  m = glm(data=ss, Acc~Distance,
          #offset=ss$off_set,
          family = binomial)
  # get predictions
  p = predict(m, data.frame(Distance=seq(0:34)), type="response")
  p = as.numeric(p)
  # switch point
  easy = 0.9
  hard = 0.1
  slab10 = which(abs(p-hard)==min(abs(p-hard)))
  slab90 = which(abs(p-easy)==min(abs(p-easy)))
  # add to dataset
  slabs_to_test = rbind(slabs_to_test, data.frame(Participant = x,
                                                  slab10 = slab10,
                                                  slab90 = slab90))

}

# tidy 
rm(ss, easy, hard, p, slab10, slab90, x)

# get range of values 
furthest <- max(slabs_to_test$slab10)
closest <- min(slabs_to_test$slab90)

#### get 6 random numbers from this range ####
# testing slabs 
testing_slabs <- tibble(participant = character(),
                        first = character(),
                        second = character())

# fill in table 
for(i in c(1:4)){
  participant = i 
  temp = sample(closest:furthest,6)
  first = paste(temp[1],temp[2],temp[3], sep = ",")
  second = paste(temp[4],temp[5],temp[6], sep = ",")
  testing_slabs = rbind(testing_slabs, tibble(participant = i,
                                              first = first,
                                              second = second))
}

save(testing_slabs, file = "scratch/testing_slabs")



