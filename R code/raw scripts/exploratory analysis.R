library(ggplot2)

## set working directory
setwd("~/Dropbox/Data Analysis/titanic/")

## load datasets and initial data munging
data <-read.csv("./data/raw data/train.csv")
data$Survived <- as.factor(data$Survived)
data$Pclass <- as.factor(data$Pclass)
data$Name <- as.character(data$Name)
data$Ticket <- as.integer(data$Ticket)


## EXPLORATORY ANALYSIS
## variable 'Age'
# it doesn't have a normal distribution, and has 177 NAs
summary(data$Age)

par(mfrow = c(1, 2))
hist(data$Age, breaks=100)
plot(density(data$Age, na.rm=T))

# filtering by 'Survived' we can see both groups have approximately the same median, but 
# the shape is different. 'Non survived' is right skewed, whereas 'Survived' is
# left skewed.
qplot(Survived, Age, geom="boxplot", data=data)


## variable 'Pclass'
# 3rd class represents more than half of the total population (55.11%)
summary(data$Pclass)
ggplot(data, aes(x=Pclass)) + geom_bar()

# interestingly 1st class population is considerably older than the rest. This might be a
# potential confounder!
qplot(Pclass, Age, geom="boxplot", data=data)


## variable 'Ticket'
# maybe trivial, because the ticket number doesn't seem to be a relevant covariate, however
# apparently there are tickets which have been issued for more than one person (group
# tickets for families, maybe? please investigate)
hist(table(data$Ticket))