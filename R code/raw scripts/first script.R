library(ggplot2)

## set working directory
setwd("~/Dropbox/Data Analysis/titanic/")

## load datasets
data <-read.csv("./data/raw data/train.csv")

str(data)

## data munging
data$Survived <- as.factor(data$Survived)
data$Pclass <- as.factor(data$Pclass)
data$Name <- as.character(data$Name)
data$Ticket <- as.integer(data$Ticket)

## exploratory analysis
## variable "Age"
par(mfrow = c(1, 2))
hist(data$Age, breaks=100)
plot(density(data$Age, na.rm=T))

# interestingly althogh the median is similar on both groups (survived and died) the
# distributions are skewed towards different sides! 
qplot(Survived, Age, geom="boxplot", data=data)



###########

## variable "Ticket"
# maybe trivial, because the ticket number doesn't seem to be a relevant covariate, however
# apparently there are tickets which have been issued for more than one person (group
# tickets for families, maybe? please investigate)
hist(table(data$Ticket))