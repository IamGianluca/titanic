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
                       Ticket = as.integer(Ticket),
                       Area = substr(Cabin, 1, 1)
)

attach(trainData)


##### MULTIVARIATE LINEAR REGRESSION #####


