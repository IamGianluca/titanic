##### LOAD RELEVANT LIBRARIES #####

library(ggplot2)
library(plyr)


##### PREPARE DATASET #####

# set working directory
setwd("~/Dropbox/data analysis/titanic/")

# load datasets and initial data munging
data <-read.csv("./data/raw data/train.csv")
data <- transform(data,
                  Survived = factor(Survived, labels = c("No", "Yes")),
                  Pclass = factor(Pclass, labels = c("1st", "2nd", "3rd")),
                  Name = as.character(Name),
                  Ticket = as.integer(Ticket),
                  Area = substr(Cabin, 1, 1)
                  )


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


## VARIABLE 'CABIN' and 'AREA'
# order data by fare. Lots of observations don't report the cabin id (but are not NAs).
# Others have multiple cabin ids or only the initial letter which should identify the
# section. Find a way to use this data! Surely the sections at the bottom of the boat have
# a higher mortal rate. It should be a better predictor than 'Fare'. However there are lots
# of missing values
newdata <- data[order(data$Fare),]
head(newdata[, c("Fare", "Cabin")], 1000)

# recoding the 'Cabin' variable extracting the 'Area' code, we can see on certain areas
# the chances to survive were higher. It's also important to notice most of the observations
# do not report the 'Cabin' variable.
summary(data$Area)
prop.table(table(data$Area, data$Survived), 1)


## VARIABLE 'EMBARKED'
# probably is a good idea exclude those cases where 'embarked' is not reported. However, 
# people embarked in Cherbourg looks have a higher survival ratio.. which is probably
# linked to the variable 'fare'..
table(data$Embarked, data$Survived)
prop.table(table(data$Embarked, data$Survived), 1)

# in fact people embarked in Cherbourg paid on average a higher price compared to the rest
# of the passengers. We used the median to evaluate it because of the presence of numerous 
# outliers on all sub-groups.
# Keep also in mind the variance of fare paid by Cherbourg's passenger is much higher too.
qplot(Embarked, Fare, geom="boxplot", data=data)
ddply(data, "Embarked", summarise, median=median(Fare, na.rm=T), sd=sd(Fare, na.rm=T))


## VARIABLE 'TICKET'
# maybe trivial, because the ticket number doesn't seem to be a relevant covariate, however
# apparently there are tickets which have been issued for more than one person (group
# tickets for families, maybe? please investigate)
hist(table(data$Ticket))
