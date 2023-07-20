#### Anca study: Optimal Accuracy script ####
# Getting optimal accuracy vs. actual 
# Also standing position 
# making these one plot together

#### libraries ####
library(tidyverse)
library(gridExtra)

#### Any constants ####
slab_size <- 0.46

#### LOAD ####
#### LOAD: part 1 #### 
df_P1 <- read.table("data/part_1/allDataAcc.txt", header = T)

# make participant a factor 
df_P1$Participant <- as.factor(df_P1$Participant)

# collapse across directions and session
df_P1 <- df_P1 %>%
  group_by(Participant, Distance) %>%
  summarise(Accuracy = sum(Accuracy))

# sort out accuracy measure 
df_P1$trials <- 24
df_P1$Acc <- df_P1$Accuracy/df_P1$trials

# do we need the offset?
df_P1$off_set <- log((1-0.01)/0.01)

#### LOAD: Part 2 ####
load("scratch/df_P2")

# get distance from each hoop 
df_P2$South_dist <- abs(df_P2$hoop_dist - df_P2$Position) 
df_P2$North_dist <- abs(df_P2$hoop_dist + df_P2$Position)

#### Get Accuracy accross distances ####
# setup the glm 
acc_sep <- tibble(Participant = character(),
                  Distance = numeric(),
                  pred_Acc = numeric())

# setup vector of slabs 
separations <- c(0:50)

# run GLM for switch points
for(p in levels(df_P1$Participant)){
  # subset 
  ss = df_P1[df_P1$Participant == p,]
  
  # run glm
  m = glm(data = ss, Acc~Distance,
          family = binomial)
  
  y = predict(m, data.frame(Distance = separations), type = "response")
  # y = as.numeric(p)
  
  # bind this to acc_sep
  acc_sep <- rbind(acc_sep, data.frame(Participant = p, 
                                       Distance = c(0:50), 
                                       pred_acc = y))
}

# tidy 
rm(m, ss, p, separations, y)

#### Sort out estimates ####
# now sort out distances part
north_acc <- acc_sep
colnames(north_acc) <- c("Participant",
                         "North_dist",
                         "pred_north")

south_acc <- acc_sep
colnames(south_acc) <- c("Participant",
                         "South_dist",
                         "pred_south")

# merge separately
df_P2 <- merge(df_P2, north_acc)
df_P2 <- merge(df_P2, south_acc)

# tidy 
rm(north_acc, south_acc)

df_P2$Exp_acc <- (df_P2$pred_north + df_P2$pred_south)/2

# remove unused columns 
df_P2 <- df_P2 %>%
  select(-c(pred_north,
            pred_south,
            Colour,
            Direction,
            South_dist,
            North_dist))

# now sort out Optimal standing pos 
df_P2$opt_pos <- 0 
df_P2$opt_pos[df_P2$hoop_dist > df_P2$switch_point] <- 1

df_P2$Opt_acc <- df_P2$Exp_acc
df_P2$Opt_acc[df_P2$opt_pos == 1] <- 0.5

# again, reduce data set 
df_P2 <- df_P2 %>%
  select(-opt_pos)

#### PLOTS ####
#### PLOTS: Each session accuracy ####
# setup data frame 
plt_dat <- df_P2 %>%
  group_by(Participant, Condition, Session) %>%
  summarise(Actual = mean(Accuracy),
            Optimal = mean(Opt_acc),
            Expected = mean(Exp_acc))

# need to sort levels 
levels(plt_dat$Condition) <- c("Control", "Primed", "Primed")
levels(plt_dat$Session) <- c("First Session", "Second Session")

# make plot 
plt <- ggplot(plt_dat, aes(Optimal, Actual, 
                           colour = Condition))
plt <- plt + theme_bw() 
plt <- plt + geom_point()
plt <- plt + geom_abline(intercept = 0, slope = 1)
plt <- plt + scale_color_ptol()
# plt <- plt + theme(legend.position = "none")
plt <- plt + facet_wrap(~ Session)
plt


#### PLOTS: Plot of standing position ####
# setup data 
plt_dat2 <- df_P2 %>%
  group_by(Participant, Session, Condition, hoop_dist) %>%
  summarise(Position = mean(Norm_pos))

# need to sort levels 
levels(plt_dat2$Condition) <- c("Control", "Primed", "Primed")
levels(plt_dat2$Session) <- c("First Session", "Second Session")

# only second session 
plt_dat2 <- plt_dat2[plt_dat2$Session == "Second Session",]

# plot 
plt2 <- ggplot(plt_dat2, aes(hoop_dist*slab_size, Position,
                             colour = Condition))
plt2 <- plt2 + theme_bw()
plt2 <- plt2 + geom_point()
plt2 <- plt2 + scale_color_ptol()
plt2 <- plt2 + geom_line(aes(group = Participant))
# plt2 <- plt2 + theme(legend.position = "bottom")
plt2$labels$x <- "Delta (metres)"
plt2$labels$y <- "Normalise standing position"
plt2

#### PLOTS: Put together ####
grid.arrange(plt, plt2, ncol = 1)


