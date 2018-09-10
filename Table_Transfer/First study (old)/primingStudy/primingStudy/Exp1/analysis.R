library(ggplot2)
library(dplyr)

dat = read.csv('results.csv')

dat$Participant = as.factor(dat$Participant)

dat$Order = as.factor(dat$Order)
levels(dat$Order) = c('unprimed', 'primed')

# Work out who did the Reaching Task properly
dat$ReachingCorrect = ((dat$Centre == 1) + (dat$Middle == 1) + (dat$Edge==1))==3
# aggregate(ReachingCorrect ~ Participant+Order, dat, FUN="mean")


# normalise participant position
dat$Participant.pos = abs(dat$Participant.pos/dat$Hoop.dist)


# aggregate
adat = aggregate(Participant.pos ~ Participant + Order + Hoop.dist, dat, FUN=mean)

plt = ggplot(adat, aes(x=(Hoop.dist), y=Participant.pos, colour=Participant))
plt = plt + geom_point(position=position_jitter(0.1))
plt = plt + geom_smooth(method=lm, se=F)
plt = plt + facet_wrap(~Order)
plt = plt + theme_minimal()
plt = plt + scale_x_continuous(name = 'hoop position', breaks=c(5,9,13))
plt = plt + scale_y_continuous(name = 'mean participant standing position')
plt = plt + theme(legend.position="none")
ggsave("primingResults.pdf", width=12, height=6)
ggsave("primingResults.png", width=12, height=6)


dist5 = filter(adat, Hoop.dist==5)
datDiff = select(dist5, Participant, Order, Participant.pos)
names(datDiff)[3] = "pos5"
datDiff$pos13 = filter(adat, Hoop.dist==13)$Participant.pos

datDiff$pos.change = with(datDiff, pos13 - pos5)
rm(dist5)

plt = ggplot(datDiff, aes(x=Order, y=pos.change)) 
plt = plt + geom_boxplot()
plt = plt + scale_x_discrete(name="group")
plt = plt + scale_y_continuous(name="change in standing position")

plt = plt + theme_minimal()
ggsave("bxpltResults.png", width=4, height=4)
plt

t.test(
	filter(datDiff, Order=="primed")$pos.change, 
	filter(datDiff, Order=="unprimed")$pos.change,
	alternative="greater" )

library(lme4)

m = lmer(data=dat, Participant.pos ~ Order + Hoop.dist + (1|Participant))

medianDat = aggregate(Participant.pos ~ Hoop.dist + Order, dat, FUN=mean)

plt2 = ggplot(medianDat, aes(x=Hoop.dist, y=Participant.pos, fill=Order))
plt2 = plt2 + geom_bar(stat="identity", position=position_dodge())
plt2