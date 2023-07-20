#### Process Anca data ####
# This is similar to the double blind study, but incorporates 
# a measure of each participant's switching point and is within subjects 
# I think? I'll look at the data 

#### load libraries ####
library(tidyverse)

#### any functions ####

#### Constants ####
slab_size <- 0.46

pos_R = 5
pos_G = 9
pos_Y = 13
pos_B = 17

#### Switching Points ####
#### SP: Load in data ####
df_SP <- read.table("data/part_1/allDataAcc.txt", header = T)

# make participant a factor 
df_SP$Participant <- as.factor(df_SP$Participant)

# collapse across directions and session
df_SP <- df_SP %>%
  group_by(Participant, Distance) %>%
  summarise(Accuracy = sum(Accuracy))

# sort out accuracy measure 
df_SP$trials <- 24
df_SP$Acc <- df_SP$Accuracy/df_SP$trials

# do we need the offset?
df_SP$off_set <- log((1-0.01)/0.01)

#### SP: GLM ####
# make acc_sep
acc_sep <- tibble(Participant = character(),
                  switch_point = numeric())

# loop through participants data
for(i in unique(df_SP$Participant)){
  d = df_SP[df_SP$Participant == i,]
  m = glm(data = d, cbind(Accuracy, trials-Accuracy)~Distance,
          family = binomial)
  p = predict(m, data.frame(Distance = seq(1:25)), type = "response")
  p = as.numeric(p)
  slab_50 = which(abs(p-0.5)==min(abs(p-0.5)))
  acc_sep = rbind(acc_sep, data.frame(Participant = i,
                                      switch_point = slab_50))
}

# tidy
rm(d,m,i,p,slab_50)

#### Decision Session ####
# saved as two sessions so we need to combine these 
df_P2_1 <- read.csv("data/part_2/part2_1.csv")
df_P2_2 <- read.csv("data/part_2/part2_2.csv")

# add in session details 
df_P2_1$Session <- 1
df_P2_2$Session <- 2

# bind them
df_P2 <- rbind(df_P2_1, df_P2_2)

# change colnames 
colnames(df_P2) <- c("Participant",
                     "Trial",
                     "Colour",
                     "Direction",
                     "Position",
                     "Accuracy",
                     "Condition",
                     "Session")

# make session a factor
df_P2$Session <- as.factor(df_P2$Session)

# tidy 
rm(df_P2_1, df_P2_2)

# sort hoop distances 
df_P2$hoop_dist <- 0
df_P2$hoop_dist[df_P2$Colour=="R"] <- pos_R
df_P2$hoop_dist[df_P2$Colour=="G"] <- pos_G
df_P2$hoop_dist[df_P2$Colour=="Y"] <- pos_Y
df_P2$hoop_dist[df_P2$Colour=="B"] <- pos_B

# Get normalise Position 
df_P2$hoop_dist <- abs(df_P2$hoop_dist)
df_P2$Position <- abs(df_P2$Position)
df_P2$Norm_pos <- df_P2$Position/df_P2$hoop_dist

# add in switch point
df_P2 <- merge(df_P2, acc_sep)

# save this data 
save(df_P2, file = "scratch/df_P2")

# Opt_pos lines
opt_pos <- tibble(Participant = character(),
                  Condition = character(),
                  hoop_dist = numeric(),
                  opt_pos = numeric())

# setup seps 
seps <- seq(0,max(df_P2$Position), 0.1)

for(i in unique(df_P2$Participant)){
  for(c in unique(df_P2$Condition[df_P2$Participant == i])){
    d <- df_P2[df_P2$Participant == i & df_P2$Condition == c,]
    for(x in seps){
      if(x < as.numeric(d$switch_point)){
        fl <- 0
      } else if(x > as.numeric(d$switch_point)){
        fl <- 1
      }
      opt_pos <- rbind(opt_pos, data.frame(Participant = i,
                                           Condition = c,
                                           hoop_dist = x,
                                           opt_pos = fl))
    }
  }
}

# tidy 
rm(d, c, fl, i,x, seps)

#### DS: plots ####
dot_plt <- ggplot(df_P2, aes(hoop_dist,
                             Norm_pos,
                             colour = Session))
dot_plt <- dot_plt + geom_point(aes(shape = Session,
                                    colour = Session)) + 
                     scale_shape_manual(values=c(3,4))
dot_plt <- dot_plt + theme_bw()
dot_plt <- dot_plt + facet_wrap(~Condition + Participant)
dot_plt <- dot_plt + geom_path(data = opt_pos,
                               colour = "blue",
                               aes(hoop_dist,
                                   opt_pos),
                               size = 0.15)
dot_plt <- dot_plt + theme(strip.background = element_blank(),
                           strip.text.x = element_blank(),
                           legend.position = "bottom")
dot_plt$labels$x <- "Delta (Metres)"
dot_plt$labels$y <- "Normalised participant standing position"
dot_plt

#NB: Sudoku is the top row

# save
ggsave("scratch/Anca_Part2.pdf")

#### New plot: Just Second Session ####
plt_second <- df_P2[df_P2$Session == 2,]

plt <- ggplot(plt_second, aes(hoop_dist*slab_size,
                              Norm_pos,
                              colour = Condition))
plt <- plt + theme_bw()
plt <- plt + geom_jitter()
plt <- plt + theme(strip.background = element_blank(),
                   strip.text.x = element_blank(),
                   legend.position = "bottom")
plt <- plt + facet_wrap(~ Condition + Participant)
plt$labels$x <- "Delta (metres)"
plt$labels$y <- "Normalised Standing Position"
plt

# save 
ggsave("scratch/Anca_part2_session2_jitter.png")




