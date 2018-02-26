###################### analysis 0 ################
idn = read.csv(file.choose())$idn
ind = read.csv(file.choose())$ind
t.test(idn,ind, paired = TRUE)


###################### analysis 1 ################
dat=read.csv(file.choose()) # read .csv file
d=dat
head(d)

library(plyr)

d$asked_sdn = (d$asked_sdn==1)
head(d$asked_sdn)

d$asked_founders = (d$asked_founders==1)
head(d$asked_founders)

summary(d$asked_sdn)
summary(d$asked_founders)

d$asked= (d$asked_sdn | d$asked_founders)
summary(d$asked)

r=glm(asked~control, data=d, family="binomial");summary(r)
exp(coef(r))

d$social= (d$control==0)
summary(d$social)
r=glm(asked~social, data=d, family="binomial");summary(r)
exp(coef(r))
summary(d$asked)


###################### analysis 2 ################
d=read.csv(file.choose()) # read .csv file
head(d)

library(plyr)

d$asked_sdn = (d$asked_sdn==1)
head(d$asked_sdn)

d$asked_founders = (d$asked_founders==1)
head(d$asked_founders)

summary(d$asked_sdn)
summary(d$asked_founders)

d$asked= (d$asked_sdn | d$asked_founders)
summary(d$asked)

r=glm(asked~study, data=d, family="binomial");summary(r)
exp(coef(r))

summary(r)
plot(d$asked,d$study,xlab="Asking int.",ylab="Presence of collect.")
curve(predict(r,data.frame(study=x),type="resp"),add=TRUE)


###################### analysis 3 ################
d=read.csv(file.choose()) # read .csv file
head(d)

library(plyr)

d$asked = (d$asked==1)
head(d$asked)

d$accepted = (d$accepted==1)
head(d$accepted)

r=glm(accepted~study, data=d, family="binomial");summary(r)
exp(coef(r))


###################### analysis 4 ################
### Efficacy
individuals=read.csv(file.choose())$individuals
collective=read.csv(file.choose())$collective
t.test(individuals,collective, paired = FALSE)
sd(individuals)
mean(individuals)
sd(collective)
mean(collective)


###################### analysis 5 ################
f_group=read.csv(file.choose()) 
f_person=read.csv(file.choose()) 
merged = rbind(f_group, f_person)
#merged[is.na(merged)] = 0
merged=na.omit(merged)
merged$ask_collective = (merged$giver_id == 2)
merged$ask_individual = !merged$ask_collective
head(merged)
head(merged$asker_giver.reachable_max.cs)

library(ggplot2)
ggplot(merged, aes(asker_giver.reachable_max.cs, giver.all_target_max.cs)) + geom_point() + xlim(0,10) + ylim(0,10) + geom_abline(slope=1, intercept=0)
cor.test(merged$asker_giver.reachable_max.cs, merged$giver.all_target_max.cs)

l <- glm(ask_collective ~ asker_giver.reachable_max.cs, data=merged, family="binomial")
summary(l)
exp(coef(l))

l2 <- glm(ask_collective ~ giver.reachable_target_max.cs, data=merged, family="binomial")
summary(l2)
exp(coef(l2))
