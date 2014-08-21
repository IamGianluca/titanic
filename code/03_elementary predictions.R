rm(list = ls())

##### LOAD RELEVANT LIBRARIES #####

library(dplyr)
library(ggplot2)
#library(rpart)
#library(rattle)
#library(rpart.plot)
#library(RColorBrewer)

options(decimal = 2)


##### LOAD DATASET #####

setwd("~/Dropbox/Data Analysis/kaggle/titanic")

train <- read.csv("./data/train.csv")
test <- read.csv("./data/test.csv")

##### EXPLORATORY ANALYSIS #####

prop.table(table(train$Sex, train$Survived), 1)

# predict all passengers will die and first Kaggle submission
test$Survived <- 0
test$Survived[test$Sex == "Female"] <- 1

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "first_submit.csv", row.names = FALSE)

# create new variable Child and populate it
train$Child <- 0
train$Child[train$Age < 18] <- 1

# calculate the probability of survive grouping by Sex and Child
# females significantly outperform males, particularly on non child group
train %.% 
  group_by(Child, Sex) %.%
  select(Child, Sex, Survived) %.%
  summarise(Survived = sum(Survived) / length(Survived))

# create factor for Fare
train$Fare2 <- NA

for (i in 1:length(train$Fare2)) {
  if (train$Fare[i] < 10) {train$Fare2[i] <- '<10'}
  else if (train$Fare[i] >= 10 & train$Fare[i] < 20) {train$Fare2[i] <- '10-20'}
  else if (train$Fare[i] >= 20 & train$Fare[i] < 30) {train$Fare2[i] <- '20-30'}
  else {train$Fare2[i] <- '30+'}
}

# summarise survivor by Fare2, Pclass and Sex
train %.% 
  group_by(Fare2, Pclass, Sex) %.%
  summarise(Survived = sum(Survived) / n()) %.%
  arrange(Fare2, Pclass, Sex)

# predict all female survive except those with expensive tickets in 3rd class and submit 
# result to Kaggle
test$Survived <- 0
test$Survived[test$Sex == 'female'] <- 1
test$Survived[test$Pclass == 3 & test$Fare2 == '30+'] <- 0

submit <- data.frame(PassengerId = test$PassengerId, Survived = test$Survived)
write.csv(submit, file = "second_submit.csv", row.names = FALSE)
