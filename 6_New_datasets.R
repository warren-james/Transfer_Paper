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
pixVD = 0.036
options(digits = 4)

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
sep <- c(1:450)

# data frame for accuracy accross separations
acc_sep <- tibble(
  participant = character(),
  separation = numeric(),
  accuracy = numeric()
)

# loop through participants for accuracy over all separations
for (p in unique(df$participant))
{
  # general linear model
  ss = df[which(df$participant==p),]
  m = glm(data=ss, accuracy~separation, family=binomial(mafc.probit(2)))
  for(i in 1:640){
    y = predict(m, data.frame(separation = i), type = "response")
    
    # add into new data frame
    acc_sep <- rbind(acc_sep, data.frame(participant = p, 
                                         separation  = i,
                                         accuracy = y))
  }
}

# participant 1 might need to be excluded

# save this 
save(acc_sep, file = "scratch/acc_sep")

# tidy 
rm(m, ss, i, p, sep, y)

#### Part 1 plots #### 
# summray data for mean accuracy 
temp <- group_by(df, participant, separation)
agdat <- summarise(temp, meanAcc = mean(accuracy))

# tidy 
rm(temp)

# make plot
plt = ggplot(df, aes(x=separation, y=accuracy)) 
plt = plt + stat_smooth(colour="black", method=glm, method.args = list(family=binomial(mafc.probit(2))), se=F, fullrange=TRUE) 
plt = plt + geom_point(data=agdat, aes(x=separation, y=meanAcc))
plt = plt + facet_wrap(~participant) + theme_minimal()
plt = plt + scale_y_continuous(name="proportion correct", breaks=c(0.25, 0.5, 0.75, 1.0))
plt = plt + scale_x_continuous(name="separation (pixels for now)", limits=c(0,450), breaks=c(0,150,300,450))
# ggsave("scratch/Part_1_Plots.pdf")
# or as png?
# ggsave("scratch/Part_1_Plots.png")

# tidy 
rm(plt, agdat)

#### part 2 data ####
# lod in part 2 data with switch points 
load("scratch/Elle_switch_nar_data")

# add in expected accuracy column given separation
switch_df <- merge(acc_sep, switch_df, by = c("participant", "separation"))

# reorder 
switch_df <- select(switch_df,
                    participant,
                    part,
                    block,
                    separation,
                    fixated_box,
                    correct,
                    switch_point,
                    condition, 
                    centre,
                    accuracy)

# probably want to round switch_point 
switch_df$switch_point <- as.numeric(switch_df$switch_point)
switch_df$switch_point <- round(switch_df$switch_point)

# NB: as.factor() then as.numeric() will produce a number for each level
#     Use this to transform data so it's standardised accross participants
#     order of levels is important though so be careful
#     might need to reorder the levels before you do this 
#     Though there seem to be nine levels for each participant, not 7?


#### dataset for accuracy given fixation location ####
# gives accuracy given where they looked based on separation
switch_df$act_acc <- switch_df$accuracy
switch_df$act_acc[switch_df$centre == 1] <- 0.75

#### make dataset for optimal accuracy ####
# first make opt fixation location
switch_df$opt_fix <- 0 
switch_df$opt_fix[switch_df$separation > switch_df$switch_point] <- 1

# now give accuracy if they had followed this strategy
switch_df$opt_acc <- switch_df$accuracy
switch_df$opt_acc[switch_df$opt_fix == 1] <- 0.75






