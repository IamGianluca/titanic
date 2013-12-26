##### LOAD RELEVANT LIBRARIES #####

library(ggplot2)


##### PREPARE DATASET #####

# set working directory
setwd("~/Dropbox/Data Analysis/titanic/")

# load datasets and initial data munging
data <-read.csv("./data/raw data/train.csv")
data$Survived <- as.factor(data$Survived)
data$Pclass <- as.factor(data$Pclass)
data$Name <- as.character(data$Name)
data$Ticket <- as.integer(data$Ticket)


##### EXPLORATORY ANALYSIS #####

## VARIABLE 'AGE'
# it doesn't have a normal distribution, and has 177 NAs
summary(data$Age)

par(mfrow = c(1, 2))
hist(data$Age, breaks=100)
plot(density(data$Age, na.rm=T))

# filtering by 'Survived' we can see both groups have approximately the same median, but 
# the shape is different. 'Non survived' is right skewed, whereas 'Survived' is left skewed.
qplot(Survived, Age, geom="boxplot", data=data)


## VARIABLE 'PCLASS'
# 3rd class represents more than half of the total population (55.11%)
summary(data$Pclass)
ggplot(data, aes(x=Pclass)) + geom_bar()

# survival rate is much higher on 1st class passenger and decrease consistently among each
# class. Surely this is an important covariate!
prop.table(table(data$Pclass, data$Survived), 1)
qplot(factor(Pclass), data=data, geom="bar", fill=factor(Survived)) 

# interestingly 1st class population is considerably older than the rest. This might be a
# potential confounder!
qplot(Pclass, Age, geom="boxplot", data=data)

# fares on lower classes are cheaper than those on 1st class. That's expectable, although
# there are some odd cases where 3rd and 2nd class tickets were more expensive than the 
# average 1st class ticket
qplot(Pclass, Fare, geom="boxplot", data=data)


## VARIABLE 'SEX'
# males are more numerous than females
summary(data$Sex)
qplot(factor(Sex), data=data, geom="bar", fill=factor(Sex))

# females look on average yourger than males
qplot(Sex, Age, geom="boxplot", data=data)

# famales have a much higher likelihood to survive (.74 vs .19)
sex_surv <- table(data$Sex, data$Survived)
prop.table(sex_surv, 1)
qplot(factor(Sex), data=data, geom="bar", fill=factor(Survived))


## VARIABLE 'SIBSP'
## VARIABLE 'PARCH'


## VARIABLE 'FARE'
# seems that passengers who paid a higher fare had a higher chance to survive
qplot(Age, Fare, colour=Survived, data=data)
qplot(Survived, Fare, geom="boxplot", data=data)


## VARIABLE 'CABIN'
## VARIABLE 'EMBARKED'


## VARIABLE 'TICKET'
# maybe trivial, because the ticket number doesn't seem to be a relevant covariate, however
# apparently there are tickets which have been issued for more than one person (group
# tickets for families, maybe? please investigate)
hist(table(data$Ticket))