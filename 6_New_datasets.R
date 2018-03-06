#### Creating new datasets #### 
# Level 4 Thesis by Elle
# 2017/18
# Written by Warren James
# Script for processing data to be analysed by Elle
# This script will produce datasets about:
# Expected accuracy had they performed optimally
# Expected accuracy based on fixation locations (to account for chance)
# Actual Accuracy accross blocks: 
#    - Doesn't Alasdair's script do this one?
# Proportion of time looking at side vs centre across 9 tested distances
#    - Also by block number (first half vs second half)



#### libraries needed ####
library(tidyverse)
library(reshape2)
library(psyphy)

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
# very slow loop... must be a quicker way to do this?
# the for(i in 1:640) causes it to slow... Can do this another way?
#for (p in unique(df$participant))
#{
#  # general linear model
#  ss = df[which(df$participant==p),]
#  m = glm(data=ss, accuracy~separation, family=binomial(mafc.probit(2)))
#  for(i in 1:640){
#    y = predict(m, data.frame(separation = i), type = "response")
#    
#    # add into new data frame
#    acc_sep <- rbind(acc_sep, data.frame(participant = p, 
#                                         separation  = i,
#                                         accuracy = y))
#  }
#}

# participant 1 might need to be excluded

# save this 
# save(acc_sep, file = "scratch/acc_sep")

# load in the above
load("scratch/acc_sep")

# tidy 
# rm(m, ss, i, p, sep, y)
# tidy if loop not run
rm(sep)

#### Part 1 plots #### 
# remove NA's 
df_nar <- df[complete.cases(df),]
# summary data for mean accuracy 
temp <- group_by(df_nar, participant, separation)
agdat <- summarise(temp, meanAcc = mean(accuracy))

# tidy 
rm(temp)

# make plot
plt = ggplot(df_nar, aes(x=separation, y=accuracy)) 
plt = plt + stat_smooth(colour="black", method=glm, method.args = list(family=binomial(mafc.probit(2))), se=F, fullrange=TRUE) 
plt = plt + geom_point(data=agdat, aes(x=separation, y=meanAcc))
plt = plt + facet_wrap(~participant) + theme_minimal()
plt = plt + scale_y_continuous(name="proportion correct", breaks=c(0.25, 0.5, 0.75, 1.0))
plt = plt + scale_x_continuous(name="separation (pixels for now)", limits=c(0,450), breaks=c(0,150,300,450))
# ggsave("scratch/plots/Part_1_Plots.pdf", height = 10, width = 10)
# or as png?
# ggsave("scratch/plots/Part_1_Plots.png")

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

# check switch points against accuracy 
temp <- group_by(switch_df, participant, separation)
temp <- summarise(temp, mean_acc = mean(accuracy))

switch_points <- group_by(switch_df, participant)
switch_points <- summarise(switch_points, switch_point = unique(switch_point))
temp <- merge(temp, switch_points)

# tidy 
rm(temp)

# NB: as.factor() then as.numeric() will produce a number for each level
#     Use this to transform data so it's standardised accross participants
#     order of levels is important though so be careful
#     might need to reorder the levels before you do this 
#     Though there seem to be nine levels for each participant, not 7?

#### Standardise separations ####
for(i in unique(switch_df$participant)){
  switch_df$standard_sep[switch_df$participant == i] <- as.numeric(as.factor(switch_df$separation[switch_df$participant == i]))
}

# subtract 5 to centre the switch point 
switch_df$standard_sep <- switch_df$standard_sep - 5

#tidy
rm(i)

#### column for accuracy given fixation location ####
# gives accuracy given where they looked based on separation
switch_df$act_acc <- switch_df$accuracy
switch_df$act_acc[switch_df$centre == 0] <- 0.75

#### make column for optimal accuracy ####
# first make opt fixation location
switch_df$opt_fix <- 0
# # this way gives some people opt accuracy below 0.75, may need to redefine their accuracy?
# switch_df$opt_fix[switch_df$separation > switch_df$switch_point] <- 1
# This way should work
switch_df$opt_fix[switch_df$accuracy > 0.75] <- 1


# now give accuracy if they had followed this strategy
switch_df$opt_acc <- switch_df$accuracy
switch_df$opt_acc[switch_df$opt_fix == 0] <- 0.75

# Now get Expected Accuracy 
switch_df$exp_acc <- switch_df$accuracy
switch_df$exp_acc[switch_df$centre == 0] <- 0.75

#### Accuracy: make new datasets #### 
# first, set first half vs second half 
switch_df$half[switch_df$block < 5] <- "first"
switch_df$half[switch_df$block > 4] <- "second"

#### Accuracy: Actual ####
# sort out actual accuracy 
temp <- group_by(switch_df, participant, half, standard_sep, condition)
Act_accuracy <- summarise(temp, mean_acc = mean(correct))

# define type of accuracy 
Act_accuracy$acc_type <- "Actual"

# change column names 
colnames(Act_accuracy) <- c("participant",
                            "half",
                            "standard_sep",
                            "condition",
                            "Accuracy",
                            "acc_type")


#### Accuracy: Optimal ####
# Add in optimal accuracy 
Opt_accuracy <- summarise(temp, mean_opt = mean(opt_acc))

# define type of accuracy 
Opt_accuracy$acc_type <- "Optimal"

# change column names 
colnames(Opt_accuracy) <- c("participant",
                            "half",
                            "standard_sep",
                            "condition",
                            "Accuracy",
                            "acc_type")

#### Accuracy: Expected Actual ####
# sort expected actual accuracy 
Exp_accuracy <- summarise(temp, mean_exp = mean(exp_acc))

# define type of accuracy 
Exp_accuracy$acc_type <- "Expected"

# change column names 
colnames(Exp_accuracy) <- c("participant",
                            "half",
                            "standard_sep",
                            "condition",
                            "Accuracy",
                            "acc_type")

# tidy 
rm(temp)

#### Accuracy: Combine these? ####
Elle_accuracy_data <- rbind(Exp_accuracy, Opt_accuracy, Act_accuracy)

# reorder 
Elle_accuracy_data <- select(Elle_accuracy_data, 
                             participant,
                             condition,
                             half,
                             standard_sep,
                             acc_type,
                             Accuracy)

#### Accuracy: reshaping data ####
# for all the data together
Elle_accuracy_data_wide <- dcast(Elle_accuracy_data,
                                 participant + 
                                   condition ~
                                   half +
                                   standard_sep +
                                   acc_type,
                                 value.var = "Accuracy")

# each individually 
# Actual
Act_accuracy_wide <- dcast(Act_accuracy,
                           participant + 
                             condition ~
                             half +
                             standard_sep +
                             acc_type,
                           value.var = "Accuracy")

# Optimal
Opt_accuracy_wide <- dcast(Opt_accuracy,
                           participant + 
                             condition ~
                             half +
                             standard_sep +
                             acc_type,
                           value.var = "Accuracy")

# Expected 
Exp_accuracy_wide <- dcast(Exp_accuracy,
                           participant + 
                             condition ~
                             half +
                             standard_sep +
                             acc_type,
                           value.var = "Accuracy")

# save txt files to be imported to excel 
# Act
write.table(Act_accuracy_wide, "Elle_data/Actual_Accuracy", row.names = FALSE, sep = "\t")

# Exp
write.table(Exp_accuracy_wide, "Elle_data/Expected_Accuracy", row.names = FALSE, sep = "\t")

# Opt
write.table(Opt_accuracy_wide, "Elle_data/Optimal_Accuracy", row.names = FALSE, sep = "\t")

# tidy
rm(Act_accuracy,
   Act_accuracy_wide,
   Elle_accuracy_data,
   Elle_accuracy_data_wide,
   Exp_accuracy,
   Exp_accuracy_wide,
   Opt_accuracy,
   Opt_accuracy_wide)

#### Fixation pos propportions ####
# only need optimal and actual, should be the same as above pretty much
# exception, need to do side and centre seperately
# Actual
# Get proportions
# centre
temp <- group_by(switch_df, participant, half, standard_sep, condition)
Act_fixation_pos_centre <- summarise(temp, mean_fix_pos = mean(centre))

# add box column
Act_fixation_pos_centre$box <- "Centre"

# side 
Act_fixation_pos_side <- summarise(temp, mean_fix_pos = 1 - mean(centre))

# add box column
Act_fixation_pos_side$box <- "Side"

# combine the two 
Act_fixation_pos <- rbind(Act_fixation_pos_centre, Act_fixation_pos_side)

# define type of fixation_pos 
Act_fixation_pos$fix_type <- "Actual"

# sort order
Act_fixation_pos <- select(Act_fixation_pos,
                           participant,
                           condition,
                           half,
                           standard_sep,
                           fix_type,
                           box,
                           mean_fix_pos)

# Optimal
# Get proportions
# centre
temp <- group_by(switch_df, participant, half, standard_sep, condition)
Opt_fixation_pos_centre <- summarise(temp, mean_fix_pos = mean(opt_fix))

# add box column
Opt_fixation_pos_centre$box <- "Centre"

# side 
Opt_fixation_pos_side <- summarise(temp, mean_fix_pos = 1 - mean(opt_fix))

# add box column
Opt_fixation_pos_side$box <- "Side"

# combine the two 
Opt_fixation_pos <- rbind(Opt_fixation_pos_centre, Opt_fixation_pos_side)

# define type of fixation_pos 
Opt_fixation_pos$fix_type <- "Optimal"

# tidy 
rm(temp)

# sort out the order
Opt_fixation_pos <- select(Opt_fixation_pos,
                           participant,
                           condition,
                           half,
                           standard_sep,
                           fix_type,
                           box,
                           mean_fix_pos)



#### Fixations: make datasets combined ####
Elle_fix_data <- rbind(Opt_fixation_pos, Act_fixation_pos)

#### Fixations: make wide versions ####
# Actual 
Elle_fix_actual_wide <- dcast(Act_fixation_pos,
                              participant + 
                                condition ~
                                box +
                                half +
                                standard_sep +
                                fix_type,
                                value.var = "mean_fix_pos")

# Optimal
Elle_fix_optimal_wide <- dcast(Opt_fixation_pos,
                              participant + 
                                condition ~
                                box +
                                half +
                                standard_sep +
                                fix_type,
                              value.var = "mean_fix_pos")


#### Fixations: save separate text files ####
# Optimal
write.table(Elle_fix_optimal_wide, "Elle_data/Optimal_Fixations", row.names = FALSE, sep = "\t")

# Actual
write.table(Elle_fix_actual_wide, "Elle_data/Actual_Fixations", row.names = FALSE, sep = "\t")

# tidy
rm(Act_fixation_pos,
   Act_fixation_pos_centre,
   Act_fixation_pos_side,
   Elle_fix_actual_wide,
   Elle_fix_data,
   Elle_fix_optimal_wide,
   Opt_fixation_pos,
   Opt_fixation_pos_centre,
   Opt_fixation_pos_side)


