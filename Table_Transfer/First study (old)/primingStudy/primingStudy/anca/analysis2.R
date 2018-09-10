library(ggplot2)
library(dplyr)
library(lme4)
library(car)
#  define hoop distances
R = 5
G = 9
Y = 13
B = 17

off_set = log((1-0.01)/0.01)

#  read in Accuracy data
accdat = read.csv("data/allDataAcc.txt", sep="\t")
accdat$Participant = as.factor(accdat$Participant)
accdat$Session = as.factor(accdat$Session)
#  plot acc data
accplt = ggplot(accdat, aes(x=Distance, y=Accuracy/12, colour=Session))
accplt = accplt + geom_point() + geom_smooth(method="glm", method.args = list(family = "binomial"), se=F )
accplt = accplt + facet_wrap(~Participant)
ggsave("accdat.pdf", width=8, height=8)
ggsave("accdat.png", width=8, height=8)

# Demonstrate that Session is not important
# mFull <- glmer(
# 	cbind(Accuracy, 12-Accuracy) ~ Session*Distance + (Session*Distance|Participant), 
# 	data=accdat, 
# 	family="binomial",
# 	control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun=100000)))


# mNoSession <- glmer(
# 	cbind(Accuracy, 12-Accuracy) ~ Distance + (Session*Distance|Participant), 
# 	data=accdat, 
# 	family="binomial",
# 	control=glmerControl(optimizer="bobyqa", optCtrl = list(maxfun=100000)))

# anova(mFull, mNoSession)
# rm(mFull, mNoSession)


dat1 = read.csv("data/allData1.txt", sep="\t")
dat2 = read.csv("data/allData2.txt", sep="\t")

names(dat1) = c("person", "trial", "color", "direction", "subj_pos", "accuracy", "condition")
names(dat2) = c("person", "trial", "color", "direction", "subj_pos", "accuracy", "condition")

dat1$person = as.factor(dat1$person)
dat2$person = as.factor(dat2$person)

dat1$session = 1
dat2$session = 2

dat = rbind(dat1, dat2)

dat$session = as.factor(dat$session)
dat$off_set = off_set

levels(dat$condition) = c("sudoku", "reaching", "reaching fail")
#  sort out hoop distances
dat$hoop_dist = 0
dat$hoop_dist[dat$color=="R"] = R
dat$hoop_dist[dat$color=="G"] = G
dat$hoop_dist[dat$color=="Y"] = Y
dat$hoop_dist[dat$color=="B"] = B

# normalise distances
dat$subj_pos = abs(dat$subj_pos / dat$hoop_dist)


# for each person, compute optimal stratgy
dat$centralPred = NaN
for (pp in levels(dat$person))
{
	pdat = filter(dat, person==pp)

	pdatacc = filter(accdat, Participant==pp)
	pdatacc$off_set = off_set
	accModel = glm(cbind(Accuracy, 12-Accuracy) ~ Distance, pdatacc, family="binomial")
	dat$centralPred[which(dat$person==pp)] = predict(accModel, data.frame(Distance=pdat$hoop_dist), type="response")
}
dat$opt_standing_pos = as.numeric(dat$centralPred<0.5)


plt = ggplot(dat, aes(x=hoop_dist, y=subj_pos, colour=session)) + geom_point(position=position_jitter(height=0, width=0.5))
plt = plt + geom_smooth(colour="black", aes(y=as.numeric(opt_standing_pos)), method="glm", method.args = list(family = "binomial"), se=F  )
plt = plt + facet_wrap(condition~person) + theme_light()
plt = plt + scale_x_continuous("hoop distance (slabs)")
plt = plt + scale_y_continuous("normalised subject position")
ggsave("results1.pdf", width=8, height=8)
ggsave("results1.png")

# calc optimal predicted accuracy
dat$predAcc = dat$centralPred
dat$predAcc[which(dat$centralPred<0.5)] = 0.5


dat = filter(dat, condition!="reaching fail")
dat$condition = as.factor(dat$condition)
plt = ggplot(dat, aes(x=as.factor(hoop_dist), y=subj_pos, fill=condition))
plt = plt + geom_boxplot()
plt = plt + facet_wrap(~session)
plt = plt + theme_light()
plt = plt + scale_x_discrete("hoop distance (slabs)")
plt = plt + scale_y_continuous("normalised subject position")
plt = plt + scale_fill_brewer(palette='Set2')
ggsave("results2.pdf", width=6, height=3)
ggsave("results2.png", width=6, height=3)

accdat2 = (dat %>% 
	group_by(person, session, hoop_dist) 
    %>% summarise(
    	condition = unique(condition),
    	actualAcc = mean((accuracy==1)),
    	optimalAcc = mean(predAcc),
    	differencePercent = 100*(optimalAcc-actualAcc)))

plt = ggplot(accdat2, aes(x=differencePercent, fill=session,))
plt = plt + geom_density(alpha=0.5)
plt = plt + facet_wrap(~condition)
plt


plt = ggplot(accdat2, aes(x=as.factor(hoop_dist), y=differencePercent)) + geom_boxplot()

library(lme4)
library(car)
m = lmer(differencePercent ~ session * condition + (1|person), accdat2)

