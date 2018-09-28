#### Non-naive participants ####

#### Packages ####
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
# # data frame for accuracy accross separations
# acc_sep <- tibble(
#   participant = character(),
#   separation = numeric(),
#   accuracy = numeric()
# )
#
# # loop through participants for accuracy over all separations
# for (p in unique(df_part1$participant))
# {
#  # general linear model
#  ss = df_part1[which(df_part1$participant==p),]
#  m = glm(data=ss, accuracy~separation, family=binomial(mafc.probit(2)))
#  for(i in 1:640){
#    y = predict(m, data.frame(separation = i), type = "response")
# 
#    # add into new data frame
#    acc_sep <- rbind(acc_sep, data.frame(participant = p,
#                                         separation  = i,
#                                         accuracy = y))
#  }
# }
# 
# # save this 
# save(acc_sep, file = "scratch/acc_sep")

# load in accuracy over distance
load("scratch/acc_sep")

#### Part 1 plots #### 
# remove NA's 
df_nar <- df_part1[complete.cases(df_part1),]

# summary data for mean accuracy 
agdat <- df_nar %>%
  group_by(participant, separation) %>%
  summarise(meanAcc = mean(accuracy))

# make plot
plt <- ggplot(df_nar, aes(x=get_VisDegs(separation/ppcm,Screen_dist),
                          y=accuracy)) 
plt <- plt + stat_smooth(method=glm,
                        method.args = list(family=binomial(mafc.probit(2))),
                        se=F,
                        fullrange=TRUE) 
plt <- plt + geom_point(data=agdat,
                       aes(x=get_VisDegs(separation/ppcm,Screen_dist),
                           y=meanAcc))
plt <- plt + facet_wrap(~participant) + theme_bw()
plt <- plt + scale_y_continuous(breaks=c(0.25, 0.5, 0.75, 1.0))
plt <- plt + scale_x_continuous(breaks = c(3,6,9,12))
plt$labels$x <- "Delta (Visual Degrees)"
plt$labels$y <- "Accuracy"
plt

# save
# ggsave("scratch/plots/Part_1_Plots.png", height = 6, width = 8)

# tidy 
rm(plt, agdat)


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
switch_df$opt_fix[switch_df$accuracy > 0.75] <- 1


# now give accuracy if they had followed this strategy
switch_df$opt_acc <- switch_df$accuracy
switch_df$opt_acc[switch_df$opt_fix == 0] <- 0.75

# Now get Expected Accuracy 
switch_df$exp_acc <- switch_df$accuracy
switch_df$exp_acc[switch_df$centre == 0] <- 0.75

#### Accuracy: make new datasets #### 
# sort actual first 
Actual <- switch_df %>%
  group_by(participant) %>%
  summarise(acc_type = "Actual",
            accuracy = mean(correct))

# optimal 
Optimal <- switch_df %>%
  group_by(participant) %>% 
  summarise(acc_type = "Optimal",
            accuracy = mean(opt_acc))

# expected 
Expected <- switch_df %>%
  group_by(participant) %>%
  summarise(acc_type = "Expected",
           accuracy = mean(exp_acc))

# combine them 
accuracy_data <- rbind(Actual, Expected)
accuracy_data <- rbind(accuracy_data, Optimal)

# save this 
save(accuracy_data, file = "scratch/accuracy_data")
