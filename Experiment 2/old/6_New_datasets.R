#### Creating new datasets #### 

#### libraries needed ####
library(tidyverse)
library(reshape2)
library(psyphy)

#### any functions #### 
get_VisDegs <- function(separation,distance){
  ((2*atan2(separation,(2*distance)))*180)/pi
}

#### Constants ####
Screen_dist <- 53
ppcm <- 1920/54

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

df$participant <- as.numeric(as.factor(df$participant))

#### Calculate curve for accuracy across distances ####
# sep <- c(1:450)
# 
# # data frame for accuracy accross separations
# acc_sep <- tibble(
#   participant = character(),
#   separation = numeric(),
#   accuracy = numeric()
# )
#
# # loop through participants for accuracy over all separations
# for (p in unique(df$participant))
# {
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
# }

# save this 
# save(acc_sep, file = "scratch/acc_sep")

# load in the above
load("scratch/acc_sep")

# tidy 
# rm(m, ss, i, p, sep, y)

#### Part 1 plots #### 
# remove NA's 
df_nar <- df[complete.cases(df),]

#### get condition ####
df_nar$condition[df_nar$participant %% 2 == 0] <- "Primed"
df_nar$condition[df_nar$participant %% 2 == 1] <- "Control"

df_nar$condition <- as.factor(df_nar$condition)
df_nar$condition <- factor(df_nar$condition, levels(df_nar$condition)[c(2,1)])

# summary data for mean accuracy 
agdat <- df_nar %>%
  group_by(participant, separation, condition) %>%
  summarise(meanAcc = mean(accuracy))

# make plot
plt <- ggplot(df_nar, aes(get_VisDegs(separation/ppcm, Screen_dist),
                         accuracy)) 
plt <- plt + theme_bw()
plt <- plt + stat_smooth(colour="black",
                        method=glm,
                        method.args = list(family=binomial(mafc.probit(2))),
                        se=F,
                        fullrange=TRUE) 
plt <- plt + geom_point(data=agdat, aes(get_VisDegs(separation/ppcm, Screen_dist),
                                       meanAcc))
plt <- plt + theme(strip.text.x = element_blank())
plt <- plt + facet_wrap(~condition + participant, ncol = 6)
plt <- plt + scale_x_continuous(breaks = c(0, 5, 10, 15))
plt <- plt + scale_y_continuous(breaks = c(.25, .5, .75, 1))
plt$labels$x <- "Delta (Visual Degrees)"
plt$labels$y <- "Accuracy"
plt

# save 
ggsave("scratch/plots/Part_1_plots.png")

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

#### Standardise separations ####
for(i in unique(switch_df$participant)){
  switch_df$standard_sep[switch_df$participant == i] <- as.numeric(as.factor(switch_df$separation[switch_df$participant == i]))
}

# subtract 5 to centre the switch point 
switch_df$standard_sep <- switch_df$standard_sep - 5

#tidy
rm(i)

# get new separations labels
new_sep_dat <- tibble(participant = character(),
                      separation = numeric(),
                      as_numbers = numeric(), #Standardised
                      as_factors = character(), # Standardised as factors
                      offset_from_sp = numeric()) # Offset from switch_point in VD

for(p in unique(switch_df$participant)){
  a <- -3
  sp <- unique(switch_df$switch_point[switch_df$participant == p])
  for(s in sort(unique(switch_df$separation[switch_df$participant == p]))){
    if(s == 284){
      new_sep <- a - 0.5
      new_sep2 <- as.factor(s)
    } else if(s == 640) {
      new_sep <- a
      new_sep2 <- as.factor(s)
    } else {
      new_sep <- a 
      new_sep2 <- as.factor(new_sep)
      a <- a + 1
    }
    new_sep3 <- get_VisDegs((s - sp)/ppcm,Screen_dist)
    new_sep_dat <- rbind(new_sep_dat, data.frame(participant = p,
                                                 separation = s,
                                                 as_numbers = new_sep,
                                                 as_factors = new_sep2,
                                                 offset_from_sp = new_sep3))
  }
}

# tidy 
rm(a, p, s, new_sep, new_sep2, new_sep3, sp)

# save this so it can be merged in other scripts etc. 
save(new_sep_dat, file = "scratch/new_slabs_dat")

# add this data into the switch_df we have 
switch_df <- merge(switch_df, new_sep_dat)

#### column for accuracy given fixation location ####
# gives accuracy given where they looked based on separation
switch_df$act_acc <- switch_df$accuracy
switch_df$act_acc[switch_df$centre == 0] <- 0.75

#### make column for optimal accuracy ####
# first make opt fixation location
switch_df$opt_fix <- 1
switch_df$opt_fix[switch_df$accuracy < 0.75] <- 0


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

# save this 
save(switch_df, file = "scratch/switch_df")

#### Accuracy: Actual ####
# sort out actual accuracy 
Act_accuracy <- switch_df %>%
  group_by(participant, half, as_numbers, as_factors, offset_from_sp, condition) %>%
  summarise(mean_acc = mean(correct))

# define type of accuracy 
Act_accuracy$acc_type <- "Actual"

# change column names 
colnames(Act_accuracy) <- c("participant",
                            "half",
                            "as_numbers",
                            "as_factors",
                            "offset_from_sp",
                            "condition",
                            "Accuracy",
                            "acc_type")


#### Accuracy: Optimal ####
# Add in optimal accuracy 
Opt_accuracy <- switch_df %>%
  group_by(participant, half, as_numbers, as_factors, offset_from_sp, condition) %>%
  summarise(mean_opt = mean(opt_acc))

# define type of accuracy 
Opt_accuracy$acc_type <- "Optimal"

# change column names 
colnames(Opt_accuracy) <- c("participant",
                            "half",
                            "as_numbers",
                            "as_factors",
                            "offset_from_sp",
                            "condition",
                            "Accuracy",
                            "acc_type")

#### Accuracy: Expected Actual ####
# sort expected actual accuracy 
Exp_accuracy <- switch_df %>%
  group_by(participant, half, as_numbers, as_factors, offset_from_sp, condition) %>%
  summarise(mean_exp = mean(exp_acc))

# define type of accuracy 
Exp_accuracy$acc_type <- "Expected"

# change column names 
colnames(Exp_accuracy) <- c("participant",
                            "half",
                            "as_numbers",
                            "as_factors",
                            "offset_from_sp",
                            "condition",
                            "Accuracy",
                            "acc_type")

#### Accuracy: Combine these? ####
Elle_accuracy_data <- rbind(Exp_accuracy, Opt_accuracy, Act_accuracy)
save(Elle_accuracy_data, file = "scratch/Elle_acc_dat")

