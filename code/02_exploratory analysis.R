rm(list = ls())

##### LOAD RELEVANT LIBRARIES AND OPTIONS #####

library(ggplot2)
library(dplyr)

options(digits=2)


##### LOAD AND CLEANING DATASET #####

setwd("~/Dropbox/Data Analysis/kaggle/titanic")
load("./data/tiny dataset.Rdata")

attach(train)

##### EXPLORATORY ANALYSIS #####

## VARIABLE 'AGE'
# Age doesn't follow a normal distribution
summary(Age)
ggplot(train, aes(x = Age)) + geom_density()

# filtering by 'Survived' we can see both groups have approximately the same median, but 
# the shape is different. 'Non survived' is right skewed, whereas 'Survived' is left skewed.
ggplot(train, aes(x = factor(Survived), y = Age)) + geom_boxplot()


## VARIABLE 'PCLASS'
# 3rd class represents more than half of the total population (55.11%)
summary(Pclass)
ggplot(train, aes(x=Pclass)) + geom_bar()

# survival rate is much higher on 1st class passenger and decrease consistently among each
# class. Surely this is an important covariate!
prop.table(table(Pclass, Survived), 1)
ggplot(train, aes(x = Pclass, fill = Survived)) + geom_bar()

# interestingly 1st class population is considerably older than the rest. This might be a
# potential confounder!
ggplot(train, aes(x = Pclass, y = Age)) + geom_boxplot()

# fares on lower classes are cheaper than those on 1st class. That's expectable, although
# there are some odd cases where 3rd and 2nd class tickets were more expensive than the 
# average 1st class ticket
ggplot(train, aes(x = Pclass, y = Fare)) + geom_boxplot()


## VARIABLE 'SEX'
# males are more numerous than females
summary(Sex)
ggplot(train, aes(x = Sex, fill = Sex)) + geom_bar()

# females look on average yourger than males
ggplot(train, aes(x = Sex, y = Age)) + geom_boxplot()

# famales have a much higher likelihood to survive (.74 vs .19)
prop.table(table(Sex, Survived), 1)
ggplot(train, aes(x = Sex, fill = Survived)) + geom_bar()


## VARIABLE 'FAMILY'
# this variable shows the family size
summary(Family)
ggplot(train, aes(x = Family, colour = Family)) + geom_density()
ggplot(train, aes(x = Family)) + geom_histogram()

ggplot(train, aes(x = Family, fill = Survived)) + geom_bar(colour = "black")

prop.table(table(Family, Survived), 1)


## VARIABLE 'FARE'
# seems that passengers who paid a higher fare had a higher chance to survive
ggplot(train, aes(x = Age, y = Fare, colour = Survived)) + geom_point()
ggplot(train, aes(x = Survived, y = Fare)) + geom_boxplot()


## VARIABLE 'EMBARKED'
# probably is a good idea exclude those cases where 'embarked' is not reported. However, 
# people embarked in Cherbourg looks have a higher survival ratio.. which is probably
# linked to the variable 'fare'..
table(Embarked, Survived)
prop.table(table(Embarked, Survived), 1)

# in fact people embarked in Cherbourg paid on average a higher price compared to the rest
# of the passengers. We used the median to evaluate it because of the presence of numerous 
# outliers on all sub-groups.
# Keep also in mind the variance of fare paid by Cherbourg's passenger is much higher too.
ggplot(train, aes(x = Embarked, y = Fare)) + geom_boxplot()
train %.%
  group_by(Embarked) %.%
  summarise(median = median(Fare, na.rm = TRUE),
            sd = sd(Fare, na.rm = TRUE))