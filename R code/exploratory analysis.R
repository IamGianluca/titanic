##### LOAD RELEVANT LIBRARIES AND OPTIONS #####

library(ggplot2)
library(plyr)

options(digits=2)


##### LOAD AND CLEANING DATASET #####

setwd("~/Dropbox/data analysis/titanic/")
trainData <-read.csv("./data/train.csv")

trainData <- transform(trainData,
                       Survived = factor(Survived, levels = c(0, 1),
                                         labels = c("No", "Yes")),
                       Pclass = factor(Pclass, levels = c(1, 2, 3),
                                       labels = c("1st", "2nd", "3rd")),
                       Name = as.character(Name),
                       Sex = factor(Sex, levels = c("male", "female"),
                                    labels = c("Male", "Female")),
                       Ticket = as.integer(Ticket),
                       Area = substr(Cabin, 1, 1)
                       )

attach(trainData)


##### EXPLORATORY ANALYSIS #####

## VARIABLE 'AGE'
# Age doesn't follow a normal distribution, and has 177 NAs
summary(Age)
ggplot(trainData, aes(x = Age)) + geom_density()

# filtering by 'Survived' we can see both groups have approximately the same median, but 
# the shape is different. 'Non survived' is right skewed, whereas 'Survived' is left skewed.
ggplot(trainData, aes(x = Survived, y = Age)) + geom_boxplot()


## VARIABLE 'PCLASS'
# 3rd class represents more than half of the total population (55.11%)
summary(Pclass)
ggplot(trainData, aes(x=Pclass)) + geom_bar()


# survival rate is much higher on 1st class passenger and decrease consistently among each
# class. Surely this is an important covariate!
prop.table(table(Pclass, Survived), 1)
ggplot(trainData, aes(x = Pclass, fill = Survived)) + geom_bar()

# interestingly 1st class population is considerably older than the rest. This might be a
# potential confounder!
ggplot(trainData, aes(x = Pclass, y = Age)) + geom_boxplot()


# fares on lower classes are cheaper than those on 1st class. That's expectable, although
# there are some odd cases where 3rd and 2nd class tickets were more expensive than the 
# average 1st class ticket
ggplot(trainData, aes(x = Pclass, y = Fare)) + geom_boxplot()


## VARIABLE 'SEX'
# males are more numerous than females
summary(Sex)
ggplot(trainData, aes(x = Sex, fill = Sex)) + geom_bar()


# females look on average yourger than males
ggplot(trainData, aes(x = Sex, y = Age)) + geom_boxplot()


# famales have a much higher likelihood to survive (.74 vs .19)
prop.table(table(Sex, Survived), 1)
ggplot(trainData, aes(x = Sex, fill = Survived)) + geom_bar()


## VARIABLE 'SIBSP'
# this variable reports the number of siblings/spouses aboard
# it seems that humans with 1 or 2 siblings/spouses aboard have a higher survival ratio.
# Also people with no siblings/spouses aboard have a decent survival rate, whereas big
# families report bad probabilities to survive the disaster.
# It important to note there are really few big families aboard, most people are in fact 
# alone or with just one relative
summary(SibSp)
ggplot(trainData, aes(x = SibSp)) + geom_density()
ggplot(trainData, aes(x = SibSp)) + geom_histogram()

ggplot(trainData, aes(x = SibSp, fill = Survived)) + geom_bar(colour = "black")

prop.table(table(SibSp, Survived), 1)


## VARIABLE 'PARCH'
# this variable reports the number of parents/children aboard
# Results are similar to those seen on the SibSp variable. Big families seems to have a 
# higher probability not to survive the disaster. Again, members of big families are rare
summary(Parch)
ggplot(trainData, aes(x = Parch)) + geom_density()
ggplot(trainData, aes(x = Parch, fill = Survived)) + geom_bar(colour = "black")

prop.table(table(Parch, Survived), 1)


## VARIABLE 'FARE'
# seems that passengers who paid a higher fare had a higher chance to survive
ggplot(trainData, aes(x = Age, y = Fare, colour = Survived)) + geom_point()
ggplot(trainData, aes(x = Survived, y = Fare)) + geom_boxplot()


## VARIABLE 'CABIN' and 'AREA'
# order data by fare. Lots of observations don't report the cabin id (but are not NAs).
# Others have multiple cabin ids or only the initial letter which should identify the
# section. Find a way to use this data! Surely the sections at the bottom of the boat have
# a higher mortal rate. It should be a better predictor than 'Fare'. However there are lots
# of missing values
newdata <- trainData[order(Fare),]
head(newdata[, c("Fare", "Cabin")], 1000)

# recoding the 'Cabin' variable extracting the 'Area' code, we can see on certain areas
# the chances to survive were higher. It's also important to notice most of the observations
# do not report the 'Cabin' variable.
summary(Area)
prop.table(table(Area, Survived), 1)


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
ggplot(trainData, aes(x = Embarked, y = Fare)) + geom_boxplot()
ddply(trainData, "Embarked", summarise, median = median(Fare, na.rm = TRUE),
      sd = sd(Fare, na.rm = TRUE))


## VARIABLE 'TICKET'
# maybe trivial, because the ticket number doesn't seem to be a relevant covariate, however
# apparently there are tickets which have been issued for more than one person (group
# tickets for families, maybe?)
summary(Ticket)
hist(table(Ticket))


##### DATA MANIPULATION #####

# extract FormOfAddress from Name
for (i in 1 : nrow(trainData)) {
  if (grepl("Mr\\.", trainData[i, "Name"]) == TRUE) {vec[i] <- "Mr"}
  else if (grepl("Miss\\.", trainData[i, "Name"]) == TRUE) {vec[i] <- "Miss"}
  else if (grepl("Mrs\\.", trainData[i, "Name"]) == TRUE) {vec[i] <- "Mrs"}
  else if (grepl("Ms\\.", trainData[i, "Name"]) == TRUE) {vec[i] <- "Ms"}
  else if (grepl("Don\\.", trainData[i, "Name"]) == TRUE) {vec[i] <- "Don"}
  else if (grepl("Master\\.", trainData[i, "Name"]) == TRUE) {vec[i] <- "Master"}
  else if (grepl("Rev\\.", trainData[i, "Name"]) == TRUE) {vec[i] <- "Rev"}
  else if (grepl("Mme\\.", trainData[i, "Name"]) == TRUE) {vec[i] <- "Mme"}
  else if (grepl("Dr\\.", trainData[i, "Name"]) == TRUE) {vec[i] <- "Dr"}
  else if (grepl("Sir\\.", trainData[i, "Name"]) == TRUE) {vec[i] <- "Sir"}
  else if (grepl("Lady\\.", trainData[i, "Name"]) == TRUE) {vec[i] <- "Lady"}
  else if (grepl("Mlle\\.", trainData[i, "Name"]) == TRUE) {vec[i] <- "Mlle"}
  else if (grepl("Col\\.", trainData[i, "Name"]) == TRUE) {vec[i] <- "Col"}
  
  # Jonkheer (female equivalent: Jonkvrouw) is a Dutch honorific of nobility. In Belgium,
  # the title of Écuyer (in French) or Jonkheer/Jonkvrouw (in Dutch) is the lowest title
  # within the nobility system
  else if (grepl("Jonkheer\\.", trainData[i, "Name"]) == TRUE) {vec[i] <- "Jonkheer"}
  else if (grepl("the Countess\\.", trainData[i, "Name"]) == TRUE) {vec[i] <- "Countess"}
  else if (grepl("Major\\.", trainData[i, "Name"]) == TRUE) {vec[i] <- "Major"}
  else if (grepl("Capt\\.", trainData[i, "Name"]) == TRUE) {vec[i] <- "Capt"}
  else {vec[i] <- "Boh"}
}

trainData <- as.data.frame(cbind(trainData, vec))
names(trainData)[length(trainData)] <- "FormOfAddress"
