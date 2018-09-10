#### For Josephine ####

#### libraries needed ####
library(tidyverse)
library(reshape2)
library(psyphy)

#### Any Functions ####
# function to get VisDegs 
get_VisDegs <- function(x,y){
  (2*atan2(x,(2*y)))*(180/pi)
}

#### Constants ####
Screen_dist <- 53
ppcm <- 1920/54

#### Extra information ####
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

df_part1 <- df

# load in part 2
load("scratch/df_part2")

df_part2 <- df

# tidy
rm(df)

#### Calculate curve for accuracy across distances ####
# not needed....
# sep <- c(min(df_part2$separation):(2*max(df_part2$separation)))

# data frame for accuracy accross separations
acc_sep <- tibble(
  participant = character(),
  separation = numeric(),
  accuracy = numeric()
)

# loop through participants for accuracy over all separations
for (p in unique(df_part1$participant))
{
 # general linear model
 ss = df_part1[which(df_part1$participant==p),]
 m = glm(data=ss, accuracy~separation, family=binomial(mafc.probit(2)))
 for(i in 1:640){
   y = predict(m, data.frame(separation = i), type = "response")

   # add into new data frame
   acc_sep <- rbind(acc_sep, data.frame(participant = p,
                                        separation  = i,
                                        accuracy = y))
 }
}

# save this 
save(acc_sep, file = "scratch/acc_sep")

# load in the above
# load("scratch/acc_sep")

# tidy 
# rm(m, ss, i, p, sep, y)
# tidy if loop not run
rm(sep)

#### Part 1 plots #### 
# remove NA's 
df_nar <- df_part1[complete.cases(df_part1),]
# summary data for mean accuracy 
temp <- group_by(df_nar, participant, separation)
agdat <- summarise(temp, meanAcc = mean(accuracy))

# tidy 
rm(temp)

# make plot
plt = ggplot(df_nar, aes(x=get_VisDegs(separation/ppcm,Screen_dist), y=accuracy)) 
plt = plt + stat_smooth(method=glm,
                        method.args = list(family=binomial(mafc.probit(2))),
                        se=F,
                        fullrange=TRUE) 
plt = plt + geom_point(data=agdat,
                       aes(x=get_VisDegs(separation/ppcm,Screen_dist),
                           y=meanAcc))
plt = plt + facet_wrap(~participant) + theme_bw()
plt = plt + scale_y_continuous(name="Accuracy",
                               breaks=c(0.25, 0.5, 0.75, 1.0))
plt = plt + scale_x_continuous(name="Delta (Visial Degrees)",
                               breaks = c(3,6,9,12))#, limits=c(0,450), breaks=c(0,150,300,450))
# ggsave("scratch/plots/Part_1_Josephine.pdf", height = 6, width = 6)
# or as png?
# ggsave("scratch/plots/Part_1_Plots.png", height = 6, width = 8)

# tidy 
rm(plt, agdat)

#### CHECKED UP TO HERE ####
# The below still needs to be checked to help streamline everything

#### part 2 data ####
# load in part 2 data with switch points 
load("scratch/switch_nar_data")

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

#### sort for Joespine ####
switch_df$Vis_Degs <- get_VisDegs(switch_df$separation/ppcm, Screen_dist)

#Actual
Act_Acc_vdegs <- switch_df %>% 
  group_by(participant, Vis_Degs) %>% 
  summarise(mean_acc = mean(correct))

Act_Acc_vdegs$acc_type <- "Actual"

#Optimal 
Opt_Acc_vdegs <- switch_df %>%
  group_by(participant, Vis_Degs) %>% 
  summarise(mean_acc = mean(opt_acc))

Opt_Acc_vdegs$acc_type <- "Optimal"

# combine these 
Accuracy_vdegs <- rbind(Act_Acc_vdegs, Opt_Acc_vdegs)

# save this 
write.table(Accuracy_vdegs, file = "scratch/Accuracy_Vdegs_us.txt", row.names = F, sep = "\t")


#### Accuracy: Actual ####
# sort out actual accuracy 
temp <- group_by(switch_df, participant, half, standard_sep)
Act_accuracy <- summarise(temp, mean_acc = mean(correct))

# define type of accuracy 
Act_accuracy$acc_type <- "Actual"

# change column names 
colnames(Act_accuracy) <- c("participant",
                            "half",
                            "standard_sep",
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
                            "Accuracy",
                            "acc_type")

# tidy 
rm(temp)

#### Accuracy: Combine these? ####
accuracy_data <- rbind(Exp_accuracy, Opt_accuracy, Act_accuracy)

# reorder 
accuracy_data <- select(accuracy_data,
                        participant,
                        half,
                        standard_sep,
                        acc_type,
                        Accuracy)
# save this 
save(accuracy_data, file = "scratch/accuracy_data")

#### Accuracy: reshaping data ####
# for all the data together
accuracy_data_wide <- dcast(accuracy_data,
                            participant ~
                              half +
                              standard_sep +
                              acc_type,
                            value.var = "Accuracy")

# each individually 
# Actual
Act_accuracy_wide <- dcast(Act_accuracy,
                           participant~
                             half +
                             standard_sep +
                             acc_type,
                           value.var = "Accuracy")

# Optimal
Opt_accuracy_wide <- dcast(Opt_accuracy,
                           participant ~
                             half +
                             standard_sep +
                             acc_type,
                           value.var = "Accuracy")

# Expected 
Exp_accuracy_wide <- dcast(Exp_accuracy,
                           participant ~
                             half +
                             standard_sep +
                             acc_type,
                           value.var = "Accuracy")

# save txt files to be imported to excel 
# Act
write.table(Act_accuracy_wide, "scratch/Actual_Accuracy", row.names = FALSE, sep = "\t")

# Exp
write.table(Exp_accuracy_wide, "scratch/Expected_Accuracy", row.names = FALSE, sep = "\t")

# Opt
write.table(Opt_accuracy_wide, "scratch/Optimal_Accuracy", row.names = FALSE, sep = "\t")

# tidy
rm(Act_accuracy,
   Act_accuracy_wide,
   accuracy_data,
   accuracy_data_wide,
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
temp <- group_by(switch_df, participant, half, standard_sep)
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
                           half,
                           standard_sep,
                           fix_type,
                           box,
                           mean_fix_pos)

# Optimal
# Get proportions
# centre
temp <- group_by(switch_df, participant, half, standard_sep)
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
                           half,
                           standard_sep,
                           fix_type,
                           box,
                           mean_fix_pos)



#### Fixations: make datasets combined ####
fix_data <- rbind(Opt_fixation_pos, Act_fixation_pos)

#### Fixations: make wide versions ####
# Actual 
fix_actual_wide <- dcast(Act_fixation_pos,
                         participant ~
                         box +
                           half +
                           standard_sep +
                           fix_type,
                         value.var = "mean_fix_pos")

# Optimal
fix_optimal_wide <- dcast(Opt_fixation_pos,
                          participant ~
                            box +
                            half +
                            standard_sep +
                            fix_type,
                          value.var = "mean_fix_pos")


#### Fixations: save separate text files ####
# Optimal
write.table(fix_optimal_wide, "scratch/Optimal_Fixations", row.names = FALSE, sep = "\t")

# Actual
write.table(fix_actual_wide, "scratch/Actual_Fixations", row.names = FALSE, sep = "\t")

# tidy
rm(Act_fixation_pos,
   Act_fixation_pos_centre,
   Act_fixation_pos_side,
   fix_actual_wide,
   fix_data,
   fix_optimal_wide,
   Opt_fixation_pos,
   Opt_fixation_pos_centre,
   Opt_fixation_pos_side)


